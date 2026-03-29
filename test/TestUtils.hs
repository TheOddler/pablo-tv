module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import ImageScraper (ImageScraper (..), ImageSearchFailure (..))
import Logging (Logger (..))
import Test.Syd.Discover ()
import UnliftIO (MonadUnliftIO)

newtype TestIO a = NoLogIO {runTestIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

instance Logger TestIO where
  putLogBS _ _ = pure ()

instance ImageScraper TestIO where
  tryFindImage _ = pure $ Left ImageSearchFailedScraping

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err
