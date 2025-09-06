module SafeMaths where

import Data.Int (Int32)

-- | Safe because Integer is unbounded
int32ToInteger :: Int32 -> Integer
int32ToInteger = fromIntegral
