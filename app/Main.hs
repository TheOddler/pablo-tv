module Main where

import LibMain qualified

main :: IO ()
main = do
  putStrLn "Running server..."
  LibMain.main
