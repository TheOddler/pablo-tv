{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Directory.Files (OtherFile (..), SpecialFile (..), VideoFile (..), extensionIsOneOf, fileNameIs, infoFileName, readSpecialFile, videoExtensions, watchedFileName)
import Directory.Info (DirectoryInfo (..), guessInfo)
import Directory.Watched (WatchedFiles)
import Path (Abs, Dir, Path, mkRelDir, (</>))
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
  let videoFiles = sortOn (\(VideoFile p _) -> p) videoFilesUnsorted

  -- Find all the subDirs and parse those too.
  subDirsUnsorted <- mapM readDirectory subDirPaths
  let subDirs = sortOn (.directoryPath) subDirsUnsorted

  -- Other files
  let othersUnsorted = OtherFile <$> otherPaths
  let others = sortOn (\(OtherFile p) -> p) othersUnsorted

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
          FileReadError _ -> guessedInfoFile
   in dir
        { directoryInfo = newInfo,
          directorySubDirs = guessMissingInfoRecursive <$> dir.directorySubDirs
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
