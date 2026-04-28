module Main (main) where

import LibMain qualified (main)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  LibMain.main
