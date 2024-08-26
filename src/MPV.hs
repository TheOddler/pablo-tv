{-# LANGUAGE TemplateHaskell #-}

module MPV where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, fromAbsFile, mkAbsFile, parent)
import Path.IO (ensureDir)
import System.Process (callProcess)
import Yesod.WebSockets (race_)

data MPV = MPV
  { mpvSocket :: Socket,
    mpvSocketAddress :: SockAddr
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
  putStrLn "sendingCommand:"
  print $ commandToMessage command
  sendAll mpv.mpvSocket (commandToMessage command <> "\n")

withMPV :: (MPV -> IO ()) -> IO ()
withMPV f =
  race_ runMpv $ bracket getAndConnectSoc (close . mpvSocket) f
  where
    socketPath = $(mkAbsFile "/tmp/pablo-tv/mpv.soc")
    socketPathS = fromAbsFile socketPath
    socketUnix = SockAddrUnix socketPathS

    getAndConnectSoc = do
      -- Give mpv some time to start
      threadDelay 2_000_000

      -- Then connect
      ensureDir $ parent socketPath
      soc <- socket AF_UNIX Stream 0
      connect soc socketUnix
      pure $ MPV soc socketUnix
    runMpv = callProcess "mpv" ["--force-window", "--idle", "--msg-level=all=debug", "--input-ipc-server=" ++ socketPathS]
