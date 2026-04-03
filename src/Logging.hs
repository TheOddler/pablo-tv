{-# LANGUAGE DefaultSignatures #-}

module Logging
  ( Logger (..),
    LogFunc,
    LogMsg (..),
    LogLevel (..),
    LogSlidingWindow,
    putLog,
    putLogWithMinLvlIO,
    mkLogSlidingWindow,
    readLogSlidingWindow,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BS
import Data.Queue.Bounded qualified as BQ
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import SafeIO (SafeIO (..))
import System.IO (Handle, stderr, stdout)
import UnliftIO (MonadIO (..))
import Yesod (lift)
import Yesod.WebSockets (WebSocketsT)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

data LogMsg = LogMsg
  { logMsgLevel :: LogLevel,
    logMsgTime :: UTCTime,
    logMsg :: T.Text
  }

type LogFunc m = LogMsg -> m ()

class (Monad m) => Logger m where
  putLogMsg :: LogMsg -> m ()
  getLogTime :: m UTCTime
  default getLogTime :: (SafeIO m) => m UTCTime
  getLogTime = getCurrentTime

putLog :: (Logger m) => LogLevel -> String -> m ()
putLog level msg = do
  time <- getLogTime
  putLogMsg
    LogMsg
      { logMsgLevel = level,
        logMsgTime = time,
        logMsg = T.pack msg
      }

logLevelColour :: LogLevel -> BS.ByteString
logLevelColour = \case
  Debug -> "\ESC[34m" -- Blue
  Info -> "\ESC[36m" -- Cyan
  Warning -> "\ESC[33m" -- Yellow
  Error -> "\ESC[31m" -- Red

defaultColour :: BS.ByteString
defaultColour = "\ESC[0m"

logLevelHandle :: LogLevel -> Handle
logLevelHandle = \case
  Debug -> stdout
  Info -> stdout
  Warning -> stdout
  Error -> stderr

-- | Use hPut from the bytestring module as that seems to be concurrency safe?
-- Don't use putStrLn as that isn't. Instead append the newline, even though that requires more memory.
putLogWithMinLvlIO :: (MonadIO m) => LogSlidingWindow -> LogLevel -> LogFunc m
putLogWithMinLvlIO logWindow minLogLevel msg = do
  -- Always add to the queues, regardless of minimum log level
  addLogToSlidingWindow logWindow msg
  -- Also log to strout/err
  when (msg.logMsgLevel >= minLogLevel) $
    liftIO . BS.hPut (logLevelHandle msg.logMsgLevel) $
      mconcat
        [ defaultColour,
          BS.fromString "➫ ", -- This ensure correct encoding of the arrow
          logLevelColour msg.logMsgLevel,
          T.encodeUtf8 msg.logMsg,
          defaultColour,
          "\n"
        ]

instance (Logger m, SafeIO m) => Logger (WebSocketsT m) where
  putLogMsg msg = lift $ putLogMsg msg

newtype LogSlidingWindow = LogSlidingWindow (TVar (BQ.BQueue LogMsg))

mkLogSlidingWindow :: Int -> IO LogSlidingWindow
mkLogSlidingWindow size = LogSlidingWindow <$> newTVarIO (BQ.empty size)

addLogToSlidingWindow :: (MonadIO m) => LogSlidingWindow -> LogMsg -> m ()
addLogToSlidingWindow (LogSlidingWindow queueTVar) logMsg = do
  liftIO $ atomically $ do
    queue <- readTVar queueTVar
    writeTVar queueTVar $ BQ.cons logMsg queue

readLogSlidingWindow :: (MonadIO m) => LogSlidingWindow -> m (BQ.BQueue LogMsg)
readLogSlidingWindow (LogSlidingWindow queueTVar) = liftIO $ readTVarIO queueTVar
