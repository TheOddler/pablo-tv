module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Time (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import ImageScraper (ImageScraper (..), ImageSearchFailure (..))
import Logging (Logger (..))
import SafeIO (SafeIO (..))
import Test.Syd.Discover ()
import Transformers (runSafeIOT)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (getCurrentDirectory)

newtype TestIO a = TestIO {runTestIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

instance Logger TestIO where
  putLogMsg _ = pure ()

instance ImageScraper TestIO where
  tryFindImage _ = pure $ Left ImageSearchFailedScraping

testCurrentTime :: UTCTime
testCurrentTime = UTCTime (fromOrdinalDate 2026 1) (secondsToDiffTime 0)

testModificationTime :: UTCTime
testModificationTime = UTCTime (fromOrdinalDate 2026 2) (secondsToDiffTime 0)

instance SafeIO TestIO where
  runIOSafely = runSafeIOT . runIOSafely
  unsafePinkyPromiseThisIsSafe = runSafeIOT . unsafePinkyPromiseThisIsSafe

  -- The following functions actually call the real functions, even though we mock the result. This is so that we can actually test what happens when they throw an exception.
  getCurrentTime = do
    _ <- runSafeIOT getCurrentTime
    pure testCurrentTime
  getModificationTime filePath = do
    modTime <- runSafeIOT $ getModificationTime filePath
    case modTime of
      Left err -> pure $ Left err
      Right _ -> pure $ Right testModificationTime
  getHomeDirectory = do
    _ <- runSafeIOT getHomeDirectory
    -- Pretend the current (test) folder is our home
    liftIO getCurrentDirectory

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err
