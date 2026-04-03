module Transformers where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Time qualified as Time
import GHC.Stack (callStack, prettyCallStack)
import Logging (LogFunc, LogLevel (..), LogSlidingWindow, Logger (..), putLog, putLogWithMinLvlIO)
import SafeIO (SafeIO (..))
import System.Directory qualified
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, try, tryIO)

newtype SafeIOT m a = SafeIOT {runSafeIOT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, Logger)

instance (MonadUnliftIO m, MonadThrow m, Logger m) => SafeIO (SafeIOT m) where
  runIOSafely = liftIO . tryIO
  unsafePinkyPromiseThisIsSafe f = do
    resultOrErr <- runIOSafely f
    case resultOrErr of
      Right result -> pure result
      Left err -> do
        putLog Error $ "I thought this was safe, but turned out not to be: " ++ prettyCallStack callStack
        throwM err
  getCurrentTime = liftIO Time.getCurrentTime
  getModificationTime = try . liftIO . System.Directory.getModificationTime
  getHomeDirectory = liftIO System.Directory.getHomeDirectory

newtype LoggerT m a = LoggerT {unLoggerT :: ReaderT (LogFunc m, m Time.UTCTime) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, SafeIO)

instance MonadTrans LoggerT where
  lift = LoggerT . lift

instance (Monad m) => Logger (LoggerT m) where
  getLogTime = LoggerT $ do
    (_logFunc, timeFunc) <- ask
    lift timeFunc
  putLogMsg msg = LoggerT $ do
    (logFunc, _timeFunc) <- ask
    lift $ logFunc msg

runLoggerT :: (MonadIO m) => LogSlidingWindow -> LogLevel -> LoggerT m a -> m a
runLoggerT window minLogLevel loggerT =
  runReaderT
    (unLoggerT loggerT)
    (putLogWithMinLvlIO window minLogLevel, liftIO Time.getCurrentTime)
