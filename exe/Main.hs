module Main where

import MyLib qualified

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
