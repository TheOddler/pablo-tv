module SafeMaths where

import Data.Int (Int32)

-- | Pretty safe to assume Int has at least 32 bits
int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral
