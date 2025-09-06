module Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger qualified as MonadLogger
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
  putLog level = putLogBS level . BS.fromString
  putLogBS :: LogLevel -> BS.ByteString -> m ()

instance Logger IO where
  putLogBS = putLogIO

logDuration :: (Logger m, MonadIO m) => String -> m a -> m a
logDuration label action = do
  (result, duration) <- withDuration action
  putLog Info $ label ++ " (" ++ show duration ++ ")"
  pure result

logLevelColour :: LogLevel -> BS.ByteString
logLevelColour = \case
  Debug -> "\ESC[34m" -- Blue
  Info -> "\ESC[36m" -- Cyan
  Warning -> "\ESC[33m" -- Yellow
  Error -> "\ESC[31m" -- Red

defaultColour :: BS.ByteString
defaultColour = "\ESC[0m"

-- | Use putStr from the bytestring module as that seems to be concurrency safe?
-- Don't use putStrLn as that isn't. Instead append the newline, even though that requires more memory.
putLogIO :: (MonadIO m) => LogLevel -> BS.ByteString -> m ()
putLogIO Debug _ = pure ()
putLogIO level msg =
  liftIO . BS.putStr $
    mconcat
      [ defaultColour,
        BS.fromString "➫ ", -- This ensure correct encoding of the arrow
        logLevelColour level,
        msg,
        defaultColour,
        "\n"
      ]

runLoggingT :: MonadLogger.LoggingT m a -> m a
runLoggingT loggingT =
  MonadLogger.runLoggingT
    loggingT
    ( \_loc _logSource logLevel logStr -> do
        putLogBS
          ( case logLevel of
              MonadLogger.LevelDebug -> Debug
              MonadLogger.LevelInfo -> Info
              MonadLogger.LevelWarn -> Warning
              MonadLogger.LevelError -> Error
              MonadLogger.LevelOther _ -> Error -- I guess better safe than sorry?
          )
          (MonadLogger.fromLogStr logStr)
    )
