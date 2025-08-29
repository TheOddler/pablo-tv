module Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BS
import Util (withDuration)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error

class (Monad m) => Logger m where
  putLog :: LogLevel -> String -> m ()

instance Logger IO where
  putLog = putLogIO

logDuration :: (Logger m, MonadIO m) => String -> m a -> m a
logDuration label action = do
  (result, duration) <- withDuration action
  putLog Info $ label ++ " (" ++ show duration ++ ")"
  pure result

logLevelColour :: LogLevel -> String
logLevelColour = \case
  Debug -> "\ESC[34m" -- Blue
  Info -> "\ESC[36m" -- Cyan
  Warning -> "\ESC[33m" -- Yellow
  Error -> "\ESC[31m" -- Red

defaultColour :: String
defaultColour = "\ESC[0m"

-- | Use putStr from the bytestring module as that seems to be concurrency safe?
-- Don't use putStrLn as that isn't. Instead append the newline, even though that requires more memory.
putLogIO :: (MonadIO m) => LogLevel -> String -> m ()
putLogIO level msg =
  liftIO . BS.putStr . BS.fromString $
    concat
      [ defaultColour,
        "➫ ",
        logLevelColour level,
        msg,
        defaultColour,
        "\n"
      ]
