module Main where

import MyLib qualified

main :: IO ()
main = do
  putStrLn "Running server:"
  MyLib.main
