{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Watched where

import Autodocodec (HasCodec (..), bimapCodec, dimapCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec, encodeYamlViaCodec)
import Control.Exception (SomeException)
import Control.Monad (foldM)
import Control.Monad.Catch (MonadCatch (..))
import Data.List.Extra (trimStart)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Map qualified as Map
import Data.Time (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import DirectoryOld (DirectoryRaw (..), isVideoFile, readDirectoryRaw)
import Foreign.C (CTime (..))
import GHC.Data.Maybe (catMaybes, firstJusts, fromMaybe)
import Path (Abs, Dir, File, Path, Rel, filename, mkRelFile, parent, (</>))
import SaferIO (FSRead (..), FSWrite (..), Logger (..), TimeRead (..))
import System.Posix qualified as Posix
import Text.Read (readMaybe)

newtype WatchedFiles = WatchedFiles
  { unWatchedFiles :: Map.Map (Path Rel File) (UTCTime, Posix.FileID)
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

watchedInfoName :: Path Rel File
watchedInfoName = $(mkRelFile "watched.yaml")

mkWatchedInfoPath :: Path Abs Dir -> Path Abs File
mkWatchedInfoPath root = root </> watchedInfoName

readWatchedInfo :: (FSRead m, Logger m) => Path Abs Dir -> m WatchedFiles
readWatchedInfo dir = do
  mInfoFile <- readFileBSSafe $ mkWatchedInfoPath dir
  case mInfoFile of
    Nothing -> pure $ WatchedFiles mempty
    Just infoFile -> do
      case eitherDecodeYamlViaCodec infoFile of
        Right info -> pure info
        Left err -> do
          logStr $ "Failed to decode watched file: " ++ show err
          pure $ WatchedFiles mempty

hasBeenWatched :: WatchedFiles -> Path a File -> Bool
hasBeenWatched (WatchedFiles watchedFiles) file =
  filename file `Map.member` watchedFiles

writeWatchedInfo :: (FSWrite m) => Path Abs Dir -> WatchedFiles -> m ()
writeWatchedInfo dir info = do
  writeFileBS (mkWatchedInfoPath dir) (encodeYamlViaCodec info)

data MarkAsWatchedResult = AlreadyWatched | MarkedAsWatched

markFileAsWatched ::
  (FSWrite m, FSRead m, TimeRead m, Logger m, MonadCatch m) =>
  Path Abs File ->
  m MarkAsWatchedResult
markFileAsWatched file = do
  let dir = parent file
  currentState <- readWatchedInfo dir
  updatedSate <- markFileAsWatched' currentState file
  cleanedState <- cleanWatchedInfo dir updatedSate
  writeWatchedInfo dir cleanedState
  pure $
    if hasBeenWatched currentState file
      then AlreadyWatched
      else MarkedAsWatched

-- | This is a version of markFileAsWatched that doesn't read nor write the
-- file to disk.
markFileAsWatched' ::
  (FSRead m, TimeRead m) =>
  WatchedFiles ->
  Path Abs File ->
  m WatchedFiles
markFileAsWatched' (WatchedFiles startState) file = do
  stats <- getFileStatus file
  time <- getCurrentTime
  let newState = Map.insert (filename file) (time, Posix.fileID stats) startState
  pure $ WatchedFiles newState

-- | Returns how many files were marked as watched. This could be lower than
-- the number of files in the directory if some files were already marked as
-- watched.
markAllAsWatched ::
  (FSWrite m, FSRead m, TimeRead m, Logger m, MonadCatch m) =>
  Path Abs Dir ->
  m Int
markAllAsWatched dir = do
  currentState <- readWatchedInfo dir
  dirRaw <- readDirectoryRaw dir
  let files = dirRaw.directoryVideoFiles
      filesAbs = (dir </>) <$> files
  updatedSate <- foldM markFileAsWatched' currentState filesAbs
  cleanedState <- cleanWatchedInfo dir updatedSate
  writeWatchedInfo dir cleanedState

  let watchedOld = Map.size $ unWatchedFiles currentState
      watchedNew = Map.size $ unWatchedFiles cleanedState
  pure $ watchedNew - watchedOld

data MarkAsUnwatchedResult = AlreadyUnwatched | MarkedAsUnwatched

markFileAsUnwatched ::
  (FSWrite m, FSRead m, Logger m, MonadCatch m) =>
  Path Abs File ->
  m MarkAsUnwatchedResult
markFileAsUnwatched file = do
  let dir = parent file
  currentState <- readWatchedInfo dir
  WatchedFiles cleanedState <- cleanWatchedInfo dir currentState
  let newState = Map.delete (filename file) cleanedState
  writeWatchedInfo dir $ WatchedFiles newState
  pure $
    if hasBeenWatched currentState file
      then MarkedAsUnwatched
      else AlreadyUnwatched

-- | Returns how many files were marked as unwatched.
markAllAsUnwatched ::
  (FSWrite m, FSRead m, Logger m) =>
  Path Abs Dir ->
  m Int
markAllAsUnwatched dir = do
  currentState <- readWatchedInfo dir
  writeWatchedInfo dir $ WatchedFiles Map.empty
  let countOld = Map.size $ unWatchedFiles currentState
  pure countOld

cleanWatchedInfo ::
  (MonadCatch m, FSRead m) =>
  Path Abs Dir ->
  WatchedFiles ->
  m WatchedFiles
cleanWatchedInfo dir (WatchedFiles currentState) = do
  DirectoryRaw _path videoFiles _dirs <- readDirectoryRaw dir
  let addIdToFile ::
        (MonadCatch m, FSRead m) =>
        Path Rel File ->
        m (Maybe (Posix.FileID, Path Rel File))
      addIdToFile file = do
        fileId <-
          catch
            (Just . Posix.fileID <$> getFileStatus (dir </> file))
            (\(_ :: SomeException) -> pure Nothing)
        pure $ case fileId of
          Nothing -> Nothing
          Just i -> Just (i, file)
  filesWithIds <- traverse addIdToFile videoFiles
  let idToFile :: Map.Map Posix.FileID (Path Rel File)
      idToFile = Map.fromList $ catMaybes filesWithIds

  let clean ::
        (FSRead m) =>
        (Path Rel File, (UTCTime, Posix.FileID)) ->
        m (Maybe (Path Rel File, (UTCTime, Posix.FileID)))
      clean (fileName, (watchedTime, fileId)) = do
        -- Find the real file name, if the file exists, that's the real one
        -- otherwise, see if we have a file with the same ID, if so we probably
        -- renamed the file, so take that one
        let mRealFileName =
              if fileName `elem` videoFiles
                then Just fileName
                else Map.lookup fileId idToFile

        case mRealFileName of
          -- If the file doesn't exist anywhere, we should drop it
          Nothing -> pure Nothing
          Just realFileName -> do
            -- Find real file ID, we just always read them from the file system
            -- as the given one might be outdated
            realFileId <- Posix.fileID <$> getFileStatus (dir </> realFileName)
            pure $ Just (realFileName, (watchedTime, realFileId))

  let stateAsList = Map.toList currentState
  cleanedStateAsList <- catMaybes <$> traverse clean stateAsList
  pure $ WatchedFiles $ Map.fromList cleanedStateAsList

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
readWatchedInfoAgg :: (FSRead m, Logger m) => Path Abs Dir -> m WatchedInfoAgg
readWatchedInfoAgg dir = do
  (dirs', files') <- listDirRecur dir
  let dirs = dir : dirs'
  let files = filter isVideoFile files'

  fileStatuses <- traverse getFileStatus files
  let fileStatusesNE = NE.nonEmpty fileStatuses
  let accessTimes = fmap Posix.accessTime <$> fileStatusesNE
  let modificationTimes = fmap Posix.modificationTime <$> fileStatusesNE

  watchedInfos <- traverse readWatchedInfo dirs
  let watchedInfosNE = NE.nonEmpty watchedInfos
  let watchedCount = sum $ Map.size . unWatchedFiles <$> watchedInfos
  let epoch = posixSecondsToUTCTime 0
      lastWatchedFrom :: WatchedFiles -> UTCTime
      lastWatchedFrom (WatchedFiles wfs) =
        maybe epoch NE.maximum1
          . NE.nonEmpty
          $ fst <$> Map.elems wfs
      lastWatched =
        maybe epoch (NE.maximum1 . fmap lastWatchedFrom) watchedInfosNE

  pure
    WatchedInfoAgg
      { watchedInfoLastModified = maybe (CTime minBound) NE.maximum1 modificationTimes,
        watchedInfoLastAccessed = maybe (CTime minBound) NE.maximum1 accessTimes,
        watchedInfoLastWatched = lastWatched,
        watchedInfoVideoFileCount = length files,
        watchedInfoPlayedVideoFileCount = watchedCount
      }
