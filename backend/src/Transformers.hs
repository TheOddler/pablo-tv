module Transformers where

import Control.Exception (annotateIO, try)
import Control.Exception.Annotation (ExceptionAnnotation (..))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Time qualified as Time
import Data.Word (Word32)
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Logging (LogFunc, LogLevel (..), LogSlidingWindow, Logger (..), putLogWithMinLvlIO)
import SafeIO (SafeIO (..))
import System.Directory qualified
import System.Random (randomIO)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)

newtype SafeIOT m a = SafeIOT {runSafeIOT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, Logger)

newtype BrokenPromise = BrokenPromise CallStack

instance ExceptionAnnotation BrokenPromise where
  displayExceptionAnnotation (BrokenPromise cs) =
    "I thought this was safe, but turned out not to be: " ++ prettyCallStack cs

instance (MonadIO m) => SafeIO (SafeIOT m) where
  runIOSafely = liftIO . try
  unsafePinkyPromiseThisIsSafe f = liftIO $ annotateIO (BrokenPromise callStack) f
  getCurrentTime = liftIO Time.getCurrentTime
  getModificationTime = liftIO . try . System.Directory.getModificationTime
  randomFileNameSuffix = liftIO $ show <$> (randomIO :: IO Word32)
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
