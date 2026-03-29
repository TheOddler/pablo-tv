module Transformers where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Time qualified as Time
import GHC.Stack (callStack, prettyCallStack)
import Logging (LogFunc, LogLevel (..), Logger (..), putLog, putLogWithMinLvlIO)
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

newtype LoggerT m a = LoggerT {unLoggerT :: ReaderT (LogFunc m) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, SafeIO)

instance MonadTrans LoggerT where
  lift = LoggerT . lift

instance (Monad m) => Logger (LoggerT m) where
  putLogBS lvl msg = LoggerT $ do
    logFunc <- ask
    lift $ logFunc lvl msg

runLoggerT :: (MonadIO m) => LogLevel -> LoggerT m a -> m a
runLoggerT minLogLevel loggerT = runReaderT (unLoggerT loggerT) (putLogWithMinLvlIO minLogLevel)
