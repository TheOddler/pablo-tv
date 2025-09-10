module Directory.Watched.Agg where

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Directory (Directory (..))
import Directory.Files (VideoFile (..), actualSpecialFile)
import Directory.Watched (WatchedFiles (..))
import Foreign.C (CTime (..))
import System.Posix qualified as Posix

-- | Aggregated data about a whole directory, including recursive subdirectories and files.
data WatchedInfoAgg = WatchedInfoAgg
  { -- | The last time this directory, or any of it's files/subdir (recursively) was modified.
    watchedInfoLastModified :: Posix.EpochTime,
    watchedInfoLastAccessed :: Posix.EpochTime,
    watchedInfoLastWatched :: UTCTime,
    -- | Total number of video files in this directory or any subdirs (recursively)
    watchedInfoVideoFileCount :: Int,
    -- | Count of files that were watched.
    -- Here we take the watched files as gospel, and don't check if the files still exist.
    watchedInfoPlayedVideoFileCount :: Int
  }
  deriving (Eq)

-- | Gather data about the directory and all subdirectories recursively and return the aggregated value.
readWatchedInfoAgg :: Directory -> WatchedInfoAgg
readWatchedInfoAgg dir =
  let files = allVideoFilesRecur dir
      fileStatuses = videoFileStatus <$> files
      fileStatusesNE = NE.nonEmpty fileStatuses
      accessTimes = fmap Posix.accessTime <$> fileStatusesNE
      modificationTimes = fmap Posix.modificationTime <$> fileStatusesNE

      watchedInfos = allWatchedFilesRecur dir
      watchedInfosNE = NE.nonEmpty watchedInfos
      watchedCount = sum $ Map.size . unWatchedFiles <$> watchedInfos
      epoch = posixSecondsToUTCTime 0
      lastWatchedFrom :: WatchedFiles -> UTCTime
      lastWatchedFrom (WatchedFiles wfs) =
        maybe epoch NE.maximum1
          . NE.nonEmpty
          $ fst <$> Map.elems wfs
      lastWatched =
        maybe epoch (NE.maximum1 . fmap lastWatchedFrom) watchedInfosNE
   in WatchedInfoAgg
        { watchedInfoLastModified = maybe (CTime minBound) NE.maximum1 modificationTimes,
          watchedInfoLastAccessed = maybe (CTime minBound) NE.maximum1 accessTimes,
          watchedInfoLastWatched = lastWatched,
          watchedInfoVideoFileCount = length files,
          watchedInfoPlayedVideoFileCount = watchedCount
        }

allVideoFilesRecur :: Directory -> [VideoFile]
allVideoFilesRecur = concat . getAllFlattened directoryVideoFiles

allWatchedFilesRecur :: Directory -> [WatchedFiles]
allWatchedFilesRecur = mapMaybe actualSpecialFile . getAllFlattened directoryWatched

getAllFlattened :: (Directory -> a) -> Directory -> [a]
getAllFlattened getter dir = getAllFlattened' (dir : dir.directorySubDirs) []
  where
    getAllFlattened' [] acc = acc
    getAllFlattened' (d : ds) acc = getAllFlattened' (d.directorySubDirs ++ ds) (getter d : acc)
