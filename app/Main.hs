module Main where

import LibMain qualified
import Logging (LogLevel (..), putLog)

main :: IO ()
main = do
  putLog Info "Running server..."
  LibMain.main
