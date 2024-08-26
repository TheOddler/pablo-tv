{-# LANGUAGE TemplateHaskell #-}

module MPV
  ( MPV,
    MPVCommand (..),
    sendCommand,
    withMPV,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, threadDelay, tryPutMVar, tryReadMVar, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, socket)
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, fromAbsFile, mkAbsFile, parent)
import Path.IO (ensureDir)
import System.Process (ProcessHandle, spawnProcess, terminateProcess)

data MPV = MPV
  { mpvSocketAddress :: SockAddr,
    mpvSocketPath :: Path Abs File,
    mpvProcessInfo :: MVar (ProcessHandle, Socket)
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
sendCommand mpv command = do
  -- Only when opening a file we want to ensure that the mpv process is running
  -- all other commands will just be no-ops if mpv is not running
  case command of
    MPVCommandOpenFile _ -> ensureMPVRunning mpv
    _ -> pure ()

  let commandMessage = commandToMessage command
  processInfo <- tryReadMVar mpv.mpvProcessInfo
  case processInfo of
    Just (_handler, soc) -> do
      print $ "Sending command: " <> commandMessage
      sendAll soc (commandToMessage command <> "\n")
    Nothing ->
      print $ "Didn't send command because mpv is not running: " <> commandMessage

withMPV :: (MPV -> IO ()) -> IO ()
withMPV = bracket initialise cleanup
  where
    socketPath = $(mkAbsFile "/tmp/pablo-tv/mpv.soc")
    socketUnix = SockAddrUnix $ fromAbsFile socketPath

    initialise :: IO MPV
    initialise = do
      noProcess <- newEmptyMVar
      ensureDir $ parent socketPath
      pure $
        MPV
          { mpvSocketAddress = socketUnix,
            mpvSocketPath = socketPath,
            mpvProcessInfo = noProcess
          }

    cleanup :: MPV -> IO ()
    cleanup mpv = do
      processInfo <- tryTakeMVar mpv.mpvProcessInfo

      case processInfo of
        Just (handler, soc) -> do
          close soc
          terminateProcess handler
        Nothing -> pure ()

-- | Starts mpv if it is not already running
-- Because of how concurrency works there's no guarantee mpv will be running when
-- this returns, but still, in most cases it will.
ensureMPVRunning :: MPV -> IO ()
ensureMPVRunning mpv = do
  processInfo <- tryReadMVar mpv.mpvProcessInfo
  case processInfo of
    Just _ -> pure ()
    Nothing -> do
      handler <- spawnProcess "mpv" ["--force-window", "--idle", "--msg-level=all=debug", "--input-ipc-server=" ++ fromAbsFile mpv.mpvSocketPath]

      soc <- socket AF_UNIX Stream 0
      -- Give MPV some time to start and create the socket on it's end before we try to connect to it
      threadDelay 1_000_000
      connect soc mpv.mpvSocketAddress

      success <- tryPutMVar mpv.mpvProcessInfo (handler, soc)
      when (not success) $ do
        -- Looks like another thread was also starting mpv at the same time,
        -- and the other thread was just faster. So close this one again.
        close soc
        terminateProcess handler
