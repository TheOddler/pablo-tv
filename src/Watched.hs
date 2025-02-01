{-# OPTIONS_GHC -Wno-orphans #-}

module Watched where

import Autodocodec (HasCodec (..), bimapCodec, dimapCodec)
import Data.List.Extra (trimStart)
import Data.Map (Map)
import Data.Time (UTCTime (..))
import GHC.Data.Maybe (firstJusts, fromMaybe)
import Path (File, Path, Rel)
import System.Posix qualified as Posix
import Text.Read (readMaybe)

newtype WatchedFiles = WatchedFiles
  { unWatchedFiles :: Map (Path Rel File) (UTCTime, Posix.FileID)
  }
  deriving (Show, Eq)

instance HasCodec WatchedFiles where
  codec = dimapCodec WatchedFiles unWatchedFiles codec

-- | For examples of the resulting format, see the test suite.
instance HasCodec (UTCTime, Posix.FileID) where
  codec =
    bimapCodec from to codec
    where
      sep = ';'
      to (time, fileId) = show time ++ (sep : " ") ++ show fileId
      from str = do
        let (timeStr, idStr') = break (== sep) str
            idStr = dropWhile (== sep) idStr'
            time =
              firstJusts
                [ readMaybe timeStr,
                  -- A fallback where we try and just read a date
                  UTCTime <$> readMaybe timeStr <*> pure 0,
                  -- If that still fails, we try and see if it at least starts
                  -- with a date
                  let dateStr = take 10 $ trimStart timeStr
                   in UTCTime <$> readMaybe dateStr <*> pure 0
                ]
            fileId = fromMaybe 0 $ readMaybe idStr
        case time of
          Just t -> Right (t, fileId)
          Nothing ->
            Left $
              unlines
                [ "Failed to parse watched info.",
                  "Full string was: " ++ str,
                  "Time part was: " ++ timeStr,
                  "File ID part was: " ++ idStr
                ]
