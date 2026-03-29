module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Logging (Logger (..))
import Test.Syd.Discover ()
import UnliftIO (MonadUnliftIO)

newtype NoLogIO a = NoLogIO {unNoLogIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

instance Logger NoLogIO where
  putLogBS _ _ = pure ()

runNoLogIO :: NoLogIO a -> IO a
runNoLogIO = unNoLogIO

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err
