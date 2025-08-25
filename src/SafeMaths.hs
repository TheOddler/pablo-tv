module SafeMaths where

import Data.Int (Int32)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Posix (EpochTime)

-- | Safe because Integer is unbounded
int32ToInteger :: Int32 -> Integer
int32ToInteger = fromIntegral

-- | Safe because internally EpochTime is just an int
-- As instructed here: https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Clock-POSIX.html#t:POSIXTime
epochToUTCTime :: EpochTime -> UTCTime
epochToUTCTime epoch = posixSecondsToUTCTime (realToFrac epoch)
