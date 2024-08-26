{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MPV
  ( MPV,
    MPVCommand (..),
    sendCommand,
    withMPV,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar, takeMVar, threadDelay)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.List (singleton)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import GHC.Generics (Generic)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, socket)
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, fromAbsFile, mkAbsFile, parent)
import Path.IO (ensureDir)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess, terminateProcess)

type MPV = MVar (Maybe MPVInfo)

data MPVInfo = MPVInfo
  { mpvSocketAddress :: SockAddr,
    mpvSocketPath :: Path Abs File,
    mpvSocket :: Socket,
    -- | Not that even though the process might be available,
    -- it could have been closed in the background, so if you use it
    -- use `getProcessExitCode` to check if it's still running
    mpvProcessHandler :: ProcessHandle
  }

data MPVCommand
  = MPVCommandTogglePlay
  | MPVCommandChangeVolume Int
  | MPVCommandSeek Int -- seconds
  | MPVCommandOpenFile (Path Abs File)
  | MPVCommandQuit
  deriving (Show, Eq, Generic)

commandToMessages :: MPVCommand -> [BS.ByteString]
commandToMessages = \case
  MPVCommandTogglePlay -> singleton [i|{ "command": ["osd-msg-bar", "cycle", "pause"] }|]
  MPVCommandChangeVolume change ->
    singleton [i|{ "command": ["osd-msg-bar", "add", "volume", #{change}] }|]
  MPVCommandSeek change ->
    singleton [i|{ "command": ["osd-msg-bar", "seek", #{change}] }|]
  MPVCommandOpenFile path ->
    [ [i|{ "command": ["osd-msg-bar", "loadfile", "#{fromAbsFile path}"] }|],
      [i|{ "command": ["set_property", "fullscreen", true] }|],
      [i|{ "command": ["set_property", "pause", false] }|]
    ]
  MPVCommandQuit -> singleton [i|{ "command": ["quit"] }|]

sendCommand :: MPV -> MPVCommand -> IO ()
sendCommand mpv command =
  withMPVInfo mpv mpvMustBeRunning $ \MPVInfo {mpvSocket = soc} -> do
    let payload = BS8.unlines $ commandToMessages command
    BS.putStr $ "Sending command(s):\n" <> payload
    sendAll soc payload
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
    start = newMVar Nothing

    cleanup :: MPV -> IO ()
    cleanup mpv =
      takeMVar mpv >>= \case
        Nothing -> pure ()
        Just info -> do
          close info.mpvSocket
          terminateProcess info.mpvProcessHandler

-- | Run an action with the MPV process info
-- Optionally starts MPV if it's not running, otherwise doesn't run the action
withMPVInfo :: MPV -> Bool -> (MPVInfo -> IO ()) -> IO ()
withMPVInfo mpv startIfNotRunning action = modifyMVar_ mpv $ \existingInfo -> do
  processIsRunning <- case existingInfo of
    Nothing -> pure False
    Just info -> isNothing <$> getProcessExitCode info.mpvProcessHandler

  mInfo <- case (processIsRunning, startIfNotRunning) of
    (False, False) -> pure Nothing
    (False, True) -> Just <$> startAndConnectToMPV
    (True, _) -> pure existingInfo

  for_ mInfo action

  pure mInfo
  where
    socketPath = $(mkAbsFile "/tmp/pablo-tv/mpv.soc")
    socketUnix = SockAddrUnix $ fromAbsFile socketPath

    startAndConnectToMPV :: IO MPVInfo
    startAndConnectToMPV = do
      ensureDir $ parent socketPath
      handler <-
        spawnProcess
          "mpv"
          ["--force-window", "--idle", "--msg-level=all=status", "--input-ipc-server=" ++ fromAbsFile socketPath]

      soc <- socket AF_UNIX Stream 0
      -- Give MPV some time to start and create the socket on it's end before we try to connect to it
      threadDelay 1_000_000
      connect soc socketUnix

      pure
        MPVInfo
          { mpvSocketAddress = socketUnix,
            mpvProcessHandler = handler,
            mpvSocket = soc,
            mpvSocketPath = socketPath
          }
