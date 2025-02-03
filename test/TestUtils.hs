module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Control.Exception (SomeException)
import Data.List.NonEmpty qualified as NE
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseRelDir, parseRelFile)
import Test.QuickCheck (Gen, elements, listOf1)
import Test.Syd.Discover ()

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err

forceRelFile :: FilePath -> Path Rel File
forceRelFile file =
  case parseRelFile file of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f

forceRelDir :: FilePath -> Path Rel Dir
forceRelDir dir =
  case parseRelDir dir of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f

forceAbsDir :: FilePath -> Path Abs Dir
forceAbsDir file =
  case parseAbsDir file of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f

-- | This is useful because a bunch of the Path functions are like
-- MonadThrow m => something -> m (Path b t)
-- With this we can force it to use the Either Monad and force it to work.
forcePath :: Either SomeException (Path b t) -> Path b t
forcePath (Right p) = p
forcePath (Left err) = error $ "Failed forcePath: " <> show err

genDirRoot :: Gen (Path Abs Dir)
genDirRoot = pure $ forceAbsDir "/"

genDirName :: Gen (Path Rel Dir)
genDirName = do
  letters <- listOf1 $ elements ['a' .. 'z']
  pure $ forceRelDir letters

genFileName :: Gen (Path Rel File)
genFileName = do
  letters <- listOf1 $ elements ['a' .. 'z']
  pure $ forceRelFile letters

genNonEmptyOf :: Gen a -> Gen (NE.NonEmpty a)
genNonEmptyOf gen = do
  l <- listOf1 gen
  case l of
    [] -> error "Impossible"
    x : xs -> pure $ x NE.:| xs
