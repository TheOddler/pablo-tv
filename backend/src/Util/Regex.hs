module Util.Regex where

import Data.Text qualified as T
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

readInt :: T.Text -> Maybe Int
readInt = readMaybe . T.unpack

tryRegex :: T.Text -> ([T.Text] -> Maybe a) -> T.Text -> Maybe a
tryRegex source resultParser regex =
  let res :: (T.Text, T.Text, T.Text, [T.Text])
      res = source =~ regex
      (_, _, _, matches) = res
   in resultParser matches

expect1Int :: [T.Text] -> Maybe Int
expect1Int = \case
  [a] ->
    readInt a
  _ -> Nothing

expect2Ints :: [T.Text] -> Maybe (Int, Int)
expect2Ints = \case
  [a, b] ->
    (,) <$> readInt a <*> readInt b
  _ -> Nothing

expect3Ints :: [T.Text] -> Maybe (Int, Int, Int)
expect3Ints = \case
  [a, b, c] ->
    (,,) <$> readInt a <*> readInt b <*> readInt c
  _ -> Nothing
