module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Test.QuickCheck (Gen, elements, listOf1)
import Test.Syd.Discover ()

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err

genNonEmptyOf :: Gen a -> Gen (NE.NonEmpty a)
genNonEmptyOf gen = do
  l <- listOf1 gen
  case l of
    [] -> error "Impossible"
    x : xs -> pure $ x NE.:| xs

genFileNameWithExt :: Set.HashSet String -> Gen T.Text
genFileNameWithExt exts = do
  name <- listOf1 $ elements ['a' .. 'z']
  ext <- elements $ Set.toList exts -- includes the .
  pure $ T.pack $ name ++ ext
