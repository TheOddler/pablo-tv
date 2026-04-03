module SafeIO where

import Control.Exception (IOException)
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Time (UTCTime)
import GHC.Stack (HasCallStack)

-- | Allows running IO in a safe way, or run IO that we know is safe to run.
-- Also allows mocking certain IO functions for testing.
-- When implementing this, make sure to make the functions safe, otherwise code
-- will fail in unexpected ways.
class (Monad m) => SafeIO m where
  runIOSafely :: IO a -> m (Either IOException a)
  unsafePinkyPromiseThisIsSafe :: (HasCallStack) => IO a -> m a
  getCurrentTime :: m UTCTime -- Known safe, and to mock during testing
  getModificationTime :: FilePath -> m (Either IOException UTCTime) -- For mocking, as git seems to change file mod times

  -- | Technically it's possible for the home directory to not exist, but realistically
  -- that's not something I want to worry about.
  -- Also useful for mocking when testing, I can make a fake home directory for the tests.
  getHomeDirectory :: m FilePath

instance (SafeIO m) => SafeIO (ReaderT r m) where
  runIOSafely = lift . runIOSafely
  unsafePinkyPromiseThisIsSafe = lift . unsafePinkyPromiseThisIsSafe
  getCurrentTime = lift getCurrentTime
  getModificationTime = lift . getModificationTime
  getHomeDirectory = lift getHomeDirectory

resolveError :: (SafeIO m) => m (Either e a) -> (e -> m a) -> m a
resolveError f onException = do
  resultOrErr <- f
  case resultOrErr of
    Right result -> pure result
    Left e -> onException e

catchAny :: (SafeIO m) => IO a -> (IOException -> m a) -> m a
catchAny io onException = do
  resultOrErr <- runIOSafely io
  case resultOrErr of
    Right result -> pure result
    Left e -> onException e

runIOSafely_ :: (SafeIO m) => IO a -> m ()
runIOSafely_ = void . runIOSafely
