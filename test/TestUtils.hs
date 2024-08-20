module TestUtils where

-- Not actually needed but gets rid of a warning cabal gives us about sydtest-discover not being used even though it is
import Test.Syd.Discover ()

labeledExpectationFailure :: (Show err) => String -> err -> a
labeledExpectationFailure label err = error $ label ++ ": " ++ show err
