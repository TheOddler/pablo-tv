{-# OPTIONS_GHC -Wno-orphans #-}

module Directory.Watched where

import Autodocodec (HasCodec (..), bimapCodec, dimapCodec)
import Data.List (find, foldl')
import Data.List.Extra (trimStart)
import Data.Map qualified as Map
import Data.Time (UTCTime (..))
import Data.Tuple (swap)
import Directory.Files (VideoFile (..))
import GHC.Data.Maybe (firstJusts, fromMaybe, mapMaybe)
import Path (Abs, File, Path, Rel, filename)
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

hasBeenWatched :: WatchedFiles -> Path Abs File -> Bool
hasBeenWatched (WatchedFiles watchedFiles) file =
  filename file `Map.member` watchedFiles

data MarkAsWatchedResult
  = AlreadyWatched
  | MarkedAsWatched Int WatchedFiles
  | MarkAsWatchedFileDoesNotExist

markFileAsWatched ::
  Path Abs File ->
  UTCTime ->
  [VideoFile] ->
  WatchedFiles ->
  MarkAsWatchedResult
markFileAsWatched filePath time videoFiles currentState =
  case find ((== filePath) . videoFilePath) videoFiles of
    Nothing -> MarkAsWatchedFileDoesNotExist
    Just file ->
      let updatedSate = markFileAsWatched' time currentState file
          cleanedState = cleanWatchedInfo videoFiles updatedSate
       in if hasBeenWatched currentState file.videoFilePath
            then AlreadyWatched
            else MarkedAsWatched 1 cleanedState

-- | This is a version of markFileAsWatched that doesn't read nor write the
-- file to disk.
markFileAsWatched' ::
  UTCTime ->
  WatchedFiles ->
  VideoFile ->
  WatchedFiles
markFileAsWatched' time (WatchedFiles startState) file = do
  let status = file.videoFileStatus
  let newState = Map.insert (filename file.videoFilePath) (time, Posix.fileID status) startState
  WatchedFiles newState

-- | Returns how many files were marked as watched. This could be lower than
-- the number of files in the directory if some files were already marked as
-- watched.
markAllAsWatched ::
  UTCTime ->
  [VideoFile] ->
  WatchedFiles ->
  MarkAsWatchedResult
markAllAsWatched time videoFiles currentState = do
  let updatedSate = foldl' (markFileAsWatched' time) currentState videoFiles
  let cleanedState = cleanWatchedInfo videoFiles updatedSate

  let watchedOld = Map.size $ unWatchedFiles currentState
      watchedNew = Map.size $ unWatchedFiles cleanedState
      diffCount = watchedNew - watchedOld
  if currentState == cleanedState
    then AlreadyWatched
    else MarkedAsWatched diffCount cleanedState

data MarkAsUnwatchedResult
  = AlreadyUnwatched
  | MarkedAsUnwatched Int WatchedFiles
  | MarkAsUnwatchedFileDoesNotExist

markFileAsUnwatched ::
  Path Abs File ->
  [VideoFile] ->
  WatchedFiles ->
  MarkAsUnwatchedResult
markFileAsUnwatched filePath videoFiles currentState =
  case find ((== filePath) . videoFilePath) videoFiles of
    Nothing -> MarkAsUnwatchedFileDoesNotExist
    Just file ->
      let WatchedFiles cleanedState = cleanWatchedInfo videoFiles currentState
          newState = Map.delete (filename file.videoFilePath) cleanedState
       in if hasBeenWatched currentState file.videoFilePath
            then MarkedAsUnwatched 1 $ WatchedFiles newState
            else AlreadyUnwatched

-- | This essentially just return an empty state while counting the existing state
markAllAsUnwatched ::
  WatchedFiles ->
  MarkAsUnwatchedResult
markAllAsUnwatched currentState = do
  let countOld = Map.size $ unWatchedFiles currentState
  if countOld == 0
    then AlreadyUnwatched
    else MarkedAsUnwatched countOld $ WatchedFiles Map.empty

cleanWatchedInfo ::
  [VideoFile] ->
  WatchedFiles ->
  WatchedFiles
cleanWatchedInfo videoFiles (WatchedFiles currentState) = do
  let toIdAndPath :: VideoFile -> (Posix.FileID, Path Abs File)
      toIdAndPath videoFile =
        (Posix.fileID videoFile.videoFileStatus, videoFile.videoFilePath)
  let filesWithIds = toIdAndPath <$> videoFiles
  let idToFile :: Map.Map Posix.FileID (Path Abs File)
      idToFile = Map.fromList filesWithIds
  let fileToId :: Map.Map (Path Abs File) Posix.FileID
      fileToId = Map.fromList (swap <$> filesWithIds)

  let clean ::
        (Path Rel File, (UTCTime, Posix.FileID)) ->
        Maybe (Path Rel File, (UTCTime, Posix.FileID))
      clean (fileName, (watchedTime, fileId)) = do
        -- Find the real file name, if a file exists with the same name, that's the real one, otherwise, see if we have a file with the same ID, if so we probably renamed the file, so take that one
        let mRealFilePath :: Maybe (Path Abs File)
            mRealFilePath =
              case find ((== fileName) . filename) (videoFilePath <$> videoFiles) of
                Just pathWithSameName -> Just pathWithSameName
                Nothing -> Map.lookup fileId idToFile

        case mRealFilePath of
          -- If the file doesn't exist anywhere, we should drop it
          Nothing -> Nothing
          Just realFilePath -> do
            -- Find real file ID, we just always read them from the file system
            -- as the given one might be outdated
            realFileId <- Map.lookup realFilePath fileToId
            Just (filename realFilePath, (watchedTime, realFileId))

  let stateAsList = Map.toList currentState
  let cleanedStateAsList = mapMaybe clean stateAsList
  WatchedFiles $ Map.fromList cleanedStateAsList
