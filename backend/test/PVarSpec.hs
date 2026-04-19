module PVarSpec where

import Control.Exception (Exception)
import PVar
import Test.Syd
import TestUtils (TestIO (..))

data TestException = TestException
  deriving (Eq, Show)

instance Exception TestException

spec :: Spec
spec = do
  it "Starts in ready state" $ runTestIO $ do
    pVar <- newPVar (1 :: Int)
    state <- readPVarState pVar
    liftIO $ state `shouldBe` PVarReady
