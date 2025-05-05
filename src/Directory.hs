{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Directory.Files (OtherFile (..), SpecialFile (..), VideoFile (..), extensionIsOneOf, fileNameIs, infoFileName, readSpecialFile, videoExtensions, watchedFileName)
import Directory.Info (DirectoryInfo (..), guessInfo, niceDirNameT, niceFileNameT)
import Directory.Watched (WatchedFiles)
import Path (Abs, Dir, File, Path, mkRelDir, (</>))
import SaferIO (FSRead (..))

newtype RootDirectory = RootDirectory (Path Abs Dir)

getVideosDir :: (FSRead m) => m RootDirectory
getVideosDir = do
  home <- getHomeDir
  let videoDirName = $(mkRelDir "Videos")
  pure $ RootDirectory $ home </> videoDirName

data Directory = Directory
  { directoryPath :: Path Abs Dir,
    directoryInfo :: SpecialFile DirectoryInfo,
    directoryWatched :: SpecialFile WatchedFiles,
    directoryVideoFiles :: [VideoFile],
    directoryOtherFiles :: [OtherFile],
    directorySubDirs :: [Directory]
  }

readRootDirectory :: (FSRead m) => RootDirectory -> m Directory
readRootDirectory (RootDirectory path) = readDirectory path

readDirectory :: (FSRead m) => Path Abs Dir -> m Directory
readDirectory path = do
  (subDirPaths, filePaths) <- listDirAbs path
  -- Pre-process all the paths we founds
  let (videoPaths, dirInfoPaths, watchedPaths, otherPaths) =
        partition4
          -- This assumes `.yaml` is not considered a video extension, otherwise we'll never find the info or watched files
          (extensionIsOneOf videoExtensions)
          (fileNameIs infoFileName)
          (fileNameIs watchedFileName)
          filePaths

  -- Parse special files
  let mDirInfoPath = listToMaybe dirInfoPaths
  dirInfo <- maybe (pure FileDoesNotExist) readSpecialFile mDirInfoPath
  let mWatchedPath = listToMaybe watchedPaths
  watched <- maybe (pure FileDoesNotExist) readSpecialFile mWatchedPath

  -- Parse the video files
  let readVideoFile p = VideoFile p <$> getFileStatus p
  videoFilesUnsorted <- mapM readVideoFile videoPaths
  let videoFiles = smartFileSort (.videoFilePath) videoFilesUnsorted

  -- Find all the subDirs and parse those too.
  subDirsUnsorted <- mapM readDirectory subDirPaths
  let subDirs = smartDirSort subDirsUnsorted

  -- Other files
  let othersUnsorted = OtherFile <$> otherPaths
  let others = smartFileSort otherFilePath othersUnsorted

  -- Finished
  pure
    Directory
      { directoryPath = path,
        directoryInfo = dirInfo,
        directoryWatched = watched,
        directoryVideoFiles = videoFiles,
        directorySubDirs = subDirs,
        directoryOtherFiles = others
      }

-- Guessing info
guessMissingInfoRecursive :: Directory -> Directory
guessMissingInfoRecursive dir =
  let guessedInfo =
        guessInfo
          dir.directoryPath
          dir.directoryVideoFiles
          (directoryPath <$> dir.directorySubDirs)
      guessedInfoFile = case guessedInfo of
        Nothing -> FileDoesNotExist
        Just i -> FileDirty i
      newInfo =
        case dir.directoryInfo of
          -- If it exists, do nothing
          FileRead i -> FileRead i
          FileDirty i -> FileDirty i
          -- Otherwise, guess
          FileDoesNotExist -> guessedInfoFile
          FileReadFail _ _ -> guessedInfoFile
          FileReadError err -> FileReadError err
      -- Only do guesses for subDirs if we think this is just a passthrough dir.
      -- I'm not sure if we should guess if there are read-errors, but I don't think so. Only do guesses when we know for use this is a passthrough dir.
      newSubDirs = case newInfo of
        FileDoesNotExist -> guessMissingInfoRecursive <$> dir.directorySubDirs
        _ -> dir.directorySubDirs
   in dir
        { directoryInfo = newInfo,
          directorySubDirs = newSubDirs
        }

-- Helpers

-- | Partition an array in 4. It'll try the first function first, then second, then third, if all fail it'll be put in the 4th list for `other`.
partition4 :: (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a], [a])
partition4 p1 p2 p3 arr = partition4' arr ([], [], [], [])
  where
    partition4' [] acc = acc
    partition4' (x : xs) (a1, a2, a3, a4)
      | p1 x = partition4' xs (x : a1, a2, a3, a4)
      | p2 x = partition4' xs (a1, x : a2, a3, a4)
      | p3 x = partition4' xs (a1, a2, x : a3, a4)
      | otherwise = partition4' xs (a1, a2, a3, x : a4)

-- | Sorts the dirs by name, taking into account numbers properly
smartDirSort :: [Directory] -> [Directory]
smartDirSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceDirNameT a.directoryPath)
        (T.toLower $ niceDirNameT b.directoryPath)

-- | Sorts the dirs, taking into account numbers properly
smartFileSort :: (a -> Path x File) -> [a] -> [a]
smartFileSort getPath = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower . niceFileNameT $ getPath a)
        (T.toLower . niceFileNameT $ getPath b)
