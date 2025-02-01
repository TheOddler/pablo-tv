module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is

import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseRelFile)
import Test.Syd.Discover ()

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err

forceRelFile :: FilePath -> Path Rel File
forceRelFile file =
  case parseRelFile file of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f

forceAbsDir :: FilePath -> Path Abs Dir
forceAbsDir file =
  case parseAbsDir file of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f
