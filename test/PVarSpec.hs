module PVarSpec where

import Control.Exception (Exception, throwIO)
import PVar
import Test.Syd

data TestException = TestException
  deriving (Eq, Show)

instance Exception TestException

spec :: Spec
spec = do
  it "Starts in ready state" $ do
    pVar <- newPVar (1 :: Int)
    state <- readPVarState pVar
    state `shouldBe` PVarReady

  it "Cleanly handles failing IO" $ do
    pVar <- newPVar (1 :: Int)
    modifyPVar pVar "test" (\_ -> throwIO TestException)
      `shouldThrow` (== TestException)
    state <- readPVarState pVar
    state `shouldBe` PVarReady
