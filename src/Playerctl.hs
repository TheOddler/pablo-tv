-- | Module for using Playerctl (https://github.com/altdesktop/playerctl) to
-- control media players that implement the MPRIS DBus interface, which are
-- most modern media players.
-- This allows better control of media players than simulation the media keys
-- does for some actions, specifically seeking can be controlled precisely,
-- and it allows to detect when a video is started playing so we can more
-- accurately mark videos as seen.
-- At some point I might want to make an implementation where I do MPRIS calls
-- directly myself, but this is a nice abstraction for now.
module Playerctl where

import Autodocodec (HasCodec (..))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Network.HTTP (urlDecode)
import Path (Abs, File, Path, parseAbsFile)
import System.Process (callProcess, readProcessWithExitCode)
import Util (boundedEnumCodec)

data Action
  = ActionPlayPause
  | ActionStop
  | ActionNext
  | ActionPrevious
  | ActionForwardStep
  | ActionBackwardStep
  | ActionForwardJump
  | ActionBackwardJump
  deriving (Show, Eq, Bounded, Enum)

instance HasCodec Action where
  codec =
    boundedEnumCodec $ \case
      ActionPlayPause -> "playPause"
      ActionStop -> "stop"
      ActionNext -> "next"
      ActionPrevious -> "previous"
      ActionForwardStep -> "forwardStep"
      ActionBackwardStep -> "backwardStep"
      ActionForwardJump -> "forwardJump"
      ActionBackwardJump -> "backwardJump"

playerctlProcessName :: FilePath
playerctlProcessName = "playerctl"

performAction :: Action -> IO ()
performAction = callPlayerctl . actionToParams
  where
    callPlayerctl = callProcess playerctlProcessName
    actionToParams = \case
      ActionPlayPause -> ["play-pause"]
      ActionStop -> ["stop"]
      ActionNext -> ["next"]
      ActionPrevious -> ["previous"]
      ActionForwardStep -> ["position", "10+"]
      ActionBackwardStep -> ["position", "10-"]
      ActionForwardJump -> ["position", "60+"]
      ActionBackwardJump -> ["position", "60-"]

onFilePlayStarted :: (Maybe (Path Abs File) -> IO ()) -> IO ()
onFilePlayStarted callback = loop Nothing
  where
    readPlayerctl args = do
      (_exitcode, stdout, _stderr) <- readProcessWithExitCode playerctlProcessName args ""
      pure $ dropWhileEnd isSpace stdout
    loop prevFile = do
      -- Note that this url encodes stuff, so if there's a space it'll encode that as %20 for example
      encodedAnswer <- readPlayerctl ["metadata", "xesam:url"]
      let answer = urlDecode encodedAnswer
      let mFile :: Maybe (Path Abs File)
          mFile = case answer of
            "No players found" -> Nothing
            'f' : 'i' : 'l' : 'e' : ':' : '/' : '/' : path ->
              parseAbsFile path
            _ -> Nothing
      when (prevFile /= mFile) $ callback mFile
      threadDelay 5_000_000 -- Not sure what a good amount of sleep would be
      loop mFile
