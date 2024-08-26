{-# LANGUAGE TemplateHaskell #-}

module MPV
  ( MPV,
    MPVCommand (..),
    sendCommand,
    withMPV,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import GHC.Generics (Generic)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, socket)
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, fromAbsFile, mkAbsFile, parent)
import Path.IO (ensureDir)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess, terminateProcess)

type MPV = TVar MPVState

data MPVState
  = MPVUninitialised
  | -- | Make sure to retry whatever you're doing when the state is `InUse`
    -- otherwise you might f stuff up.
    -- This might be an indication I should use an MVar, not sure
    MPVInUse
  | MPVAvailable MPVAvailableInfo

data MPVAvailableInfo = MPVAvailableInfo
  { mpvSocketAddress :: SockAddr,
    mpvSocketPath :: Path Abs File,
    mpvSocket :: Socket,
    -- | Not that even though the process might be available,
    -- it could have been closed in the background, so if you use it
    -- use `getProcessExitCode` to check if it's still running
    mpvProcessHandler :: ProcessHandle
  }

data MPVCommand
  = MPVCommandTogglePaused
  | MPVCommandChangeVolume Int
  | MPVCommandSeek Int -- seconds
  | MPVCommandOpenFile (Path Abs File)
  deriving (Show, Eq, Generic)

commandToMessage :: MPVCommand -> BS.ByteString
commandToMessage = \case
  -- MPVCommandPause -> "{ \"command\": [\"set_property\", \"pause\", true] }"
  MPVCommandTogglePaused -> "{ \"command\": [\"cycle\", \"pause\"] }"
  MPVCommandChangeVolume change ->
    mconcat
      [ "{ \"command\": [\"add\", \"volume\", ",
        BS8.pack (show change),
        "] }"
      ]
  MPVCommandSeek change ->
    mconcat
      [ "{ \"command\": [\"seek\", ",
        BS8.pack (show change),
        "] }"
      ]
  MPVCommandOpenFile path ->
    mconcat
      [ "{ \"command\": [\"loadfile\", \"",
        BS8.pack (fromAbsFile path),
        "\"] }"
      ]

sendCommand :: MPV -> MPVCommand -> IO ()
sendCommand mpv command =
  withMPVAvailable mpv mpvMustBeRunning $ \MPVAvailableInfo {mpvSocket = soc} ->
    sendAll soc (commandToMessage command <> "\n")
  where
    -- Only when opening a file we want to ensure that the mpv process is running
    -- all other commands will just be no-ops if mpv is not running
    mpvMustBeRunning = case command of
      MPVCommandOpenFile _ -> True
      _ -> False

withMPV :: (MPV -> IO ()) -> IO ()
withMPV = bracket start cleanup
  where
    start :: IO MPV
    start = newTVarIO MPVUninitialised

    cleanup :: MPV -> IO ()
    cleanup mpv = do
      processInfo <- atomically $ do
        value <- readTVar mpv
        -- Can't use Eq as ProcessHandle doesn't have it derived
        case value of
          MPVInUse -> retry
          _ -> pure ()
        writeTVar mpv MPVUninitialised
        pure value

      case processInfo of
        MPVAvailable info -> do
          close info.mpvSocket
          terminateProcess info.mpvProcessHandler
        _ -> pure ()

-- | Run an action with the MPV process info
-- Optionally starts MPV if it's not running, if this is not wanted the action
-- is not run when MPV was uninitialised.
-- If MPV is in use, we wait until we can run the action.
withMPVAvailable :: MPV -> Bool -> (MPVAvailableInfo -> IO ()) -> IO ()
withMPVAvailable mpv createIfUninitialised action = do
  processInfo <- atomically $ do
    value <- readTVar mpv
    writeTVar mpv MPVInUse
    case value of
      MPVInUse -> retry
      MPVUninitialised -> pure Nothing
      MPVAvailable info -> pure $ Just info

  case processInfo of
    -- MPV was uninitialised
    Nothing | createIfUninitialised -> do
      newInfo <- startAndConnectToMPV
      action newInfo `finally` atomically (writeTVar mpv (MPVAvailable newInfo))
    Nothing -> atomically (writeTVar mpv MPVUninitialised)
    Just existingInfo -> do
      mExitCode <- getProcessExitCode existingInfo.mpvProcessHandler
      info <- case mExitCode of
        Nothing -> pure existingInfo
        Just _ -> startAndConnectToMPV
      action info `finally` atomically (writeTVar mpv (MPVAvailable info))
  where
    socketPath = $(mkAbsFile "/tmp/pablo-tv/mpv.soc")
    socketUnix = SockAddrUnix $ fromAbsFile socketPath

    startAndConnectToMPV :: IO MPVAvailableInfo
    startAndConnectToMPV = do
      ensureDir $ parent socketPath
      handler <-
        spawnProcess
          "mpv"
          ["--force-window", "--idle", "--msg-level=all=debug", "--input-ipc-server=" ++ fromAbsFile socketPath]

      soc <- socket AF_UNIX Stream 0
      -- Give MPV some time to start and create the socket on it's end before we try to connect to it
      threadDelay 1_000_000
      connect soc socketUnix

      pure
        MPVAvailableInfo
          { mpvSocketAddress = socketUnix,
            mpvProcessHandler = handler,
            mpvSocket = soc,
            mpvSocketPath = socketPath
          }
