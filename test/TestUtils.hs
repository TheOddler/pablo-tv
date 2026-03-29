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
import UnliftIO (MonadUnliftIO, tryIO)
import UnliftIO.Directory (getCurrentDirectory)

newtype TestIO a = NoLogIO {runTestIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

instance Logger TestIO where
  putLogBS _ _ = pure ()

instance ImageScraper TestIO where
  tryFindImage _ = pure $ Left ImageSearchFailedScraping

testCurrentTime :: UTCTime
testCurrentTime = UTCTime (fromOrdinalDate 2026 1) (secondsToDiffTime 0)

testModificationTime :: UTCTime
testModificationTime = UTCTime (fromOrdinalDate 2026 2) (secondsToDiffTime 0)

instance SafeIO TestIO where
  runIOSafely = liftIO . tryIO
  unsafePinkyPromiseThisIsSafe f = do
    resultOrErr <- runIOSafely f
    case resultOrErr of
      Right result -> pure result
      Left err -> throwM err
  getCurrentTime = pure testCurrentTime
  getModificationTime _ = pure $ Right testModificationTime
  getHomeDirectory = liftIO getCurrentDirectory

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err
