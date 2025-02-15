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
import System.Process (callProcess, readProcess)
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

callPlayerctl :: [String] -> IO ()
callPlayerctl = callProcess playerctlProcessName

-- | Output is returned strictly, so this is not suitable for launching
-- processes that require interaction over the standard file streams.
readPlayerctl :: [String] -> IO String
readPlayerctl args = readProcess playerctlProcessName args ""

performAction :: Action -> IO ()
performAction = callPlayerctl . actionToParams
  where
    actionToParams = \case
      ActionPlayPause -> ["play-pause"]
      ActionStop -> ["stop"]
      ActionNext -> ["next"]
      ActionPrevious -> ["previous"]
      ActionForwardStep -> ["position", "10+"]
      ActionBackwardStep -> ["position", "10-"]
      ActionForwardJump -> ["position", "60+"]
      ActionBackwardJump -> ["position", "60-"]
