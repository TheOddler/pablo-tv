{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Autodocodec.Yaml (encodeYamlViaCodec)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Directory.Files (Image (..), OtherFile (..), SpecialFile (..), VideoFile (..), extensionIsOneOf, fileNameIs, fileNameIsOneOf, imageExtensions, infoFileName, posterFileNameDefault, readSpecialFile, videoExtensions, watchedFileName)
import Directory.Info (DirectoryInfo (..), downloadInfo, guessInfo, niceDirNameT, niceFileNameT)
import Directory.Watched (WatchedFiles)
import GHC.Utils.Misc (mapFst)
import Path (Abs, Dir, File, Path, Rel, addExtension, mkRelDir, parseRelFile, toFilePath, (</>))
import SaferIO (FSRead (..), FSWrite (..), Logger (..), NetworkRead)
import TVDB (TVDBToken, downloadImage)

newtype RootDirectory = RootDirectory (Path Abs Dir)

getVideosDir :: (FSRead m) => m RootDirectory
getVideosDir = do
  home <- getHomeDir
  let videoDirName = $(mkRelDir "Videos")
  pure $ RootDirectory $ home </> videoDirName

data Directory = Directory
  { directoryPath :: Path Abs Dir,
    directoryInfo :: SpecialFile DirectoryInfo,
    directoryImage :: Maybe Image,
    directoryWatched :: SpecialFile WatchedFiles,
    directoryVideoFiles :: [VideoFile],
    directoryOtherFiles :: [OtherFile], -- Should I just bin these other files? Do I need them?
    directorySubDirs :: [Directory]
  }

readRootDirectory :: (FSRead m) => RootDirectory -> m Directory
readRootDirectory (RootDirectory path) = readDirectory path

readDirectory :: (FSRead m) => Path Abs Dir -> m Directory
readDirectory path = do
  (subDirPaths, filePaths) <- listDirAbs path
  -- Pre-process all the paths we founds
  let (videoPaths, imagePaths, otherPaths) =
        partition3
          -- This assumes `.yaml` is not considered a video extension, otherwise we'll never find the info or watched files
          (extensionIsOneOf videoExtensions)
          (extensionIsOneOf imageExtensions)
          filePaths

  -- Parse special files
  let mDirInfoPath = find (fileNameIs infoFileName) otherPaths
  dirInfo <- maybe (pure FileDoesNotExist) readSpecialFile mDirInfoPath
  let mWatchedPath = find (fileNameIs watchedFileName) otherPaths
  watched <- maybe (pure FileDoesNotExist) readSpecialFile mWatchedPath

  -- Find image
  let image = case imagePaths of
        [] -> Nothing
        (i : _is) -> Just $ ImageOnDisk i

  -- Parse the video files
  let readVideoFile p = VideoFile p <$> getFileStatus p
  videoFilesUnsorted <- mapM readVideoFile videoPaths
  let videoFiles = smartFileSortBy videoFilePath videoFilesUnsorted

  -- Find all the subDirs and parse those too.
  subDirsUnsorted <- mapM readDirectory subDirPaths
  let subDirs = smartDirSort subDirsUnsorted

  -- Other files
  let othersUnsorted =
        OtherFile
          <$> filter
            (not . fileNameIsOneOf [infoFileName, watchedFileName])
            otherPaths
  let others = smartFileSortBy otherFilePath othersUnsorted

  -- Finished
  pure
    Directory
      { directoryPath = path,
        directoryInfo = dirInfo,
        directoryImage = image,
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

-- Downloading new info from the internet

-- | Downloads info from the internet for all already existing info files.
-- This will also download a new image if none exist yet and we find one online.
-- Will not do guessing so that needs to be done as a separate step before this if we want to populate new directories too.
downloadInfoRecursive :: (NetworkRead m, Logger m) => TVDBToken -> Directory -> m Directory
downloadInfoRecursive tvdbToken dir = do
  -- Download new info if we have to
  let downloadInfo' = mapFst FileDirty . downloadInfo tvdbToken
  (newInfo, mImageUrl) <-
    case dir.directoryInfo of
      -- If there's existing info, see if it needs updating, if so, update
      FileRead i | i.directoryInfoForceUpdate == Just True -> downloadInfo' i
      FileDirty i | i.directoryInfoForceUpdate == Just True -> downloadInfo' i
      -- Otherwise do nothing
      _ -> pure (dir.directoryInfo, Nothing)

  -- Update image if we don't have any yet
  newImage <- case (dir.directoryImage, mImageUrl) of
    (Nothing, Just imgUrl) -> do
      imgOrErr <- downloadImage imgUrl
      case imgOrErr of
        Left err -> do
          logStr err
          pure dir.directoryImage
        Right (contentType, imgBytes) ->
          -- We can't write the image in this function, so keep it in memory, and we can write it in the write function for `Directory` data
          pure . Just $ ImageInMemory contentType imgBytes
    _ -> pure dir.directoryImage

  -- Recursively do all subDirs
  newSubDirs <- mapM (downloadInfoRecursive tvdbToken) dir.directorySubDirs

  pure
    dir
      { directoryInfo = newInfo,
        directoryImage = newImage,
        directorySubDirs = newSubDirs
      }

-- Writing the data back to disk

-- | Write only the data that is marked as dirty to limit how much we write
writeDirtyInfoRecursive :: (FSWrite m, Logger m) => Directory -> m ()
writeDirtyInfoRecursive dir = do
  -- Write dirty directory info
  case dir.directoryInfo of
    FileDirty i -> write infoFileName $ encodeYamlViaCodec i
    _ -> pure ()

  -- Write in-memory image to disk
  case dir.directoryImage of
    Nothing -> pure ()
    Just (ImageOnDisk _) -> pure ()
    Just (ImageInMemory contentType imgBytes) ->
      let extension' =
            if BS.isPrefixOf "image/" contentType
              then Just $ BS.drop 6 contentType
              else Nothing
          extension = if extension' == Just "jpeg" then Just "jpg" else extension'
          name = fromMaybe posterFileNameDefault $ do
            ext <- extension
            parseRelFile $ BS8.unpack $ "poster." <> ext
       in write name imgBytes

  -- Write dirty watched info
  case dir.directoryWatched of
    FileDirty i -> write watchedFileName $ encodeYamlViaCodec i
    _ -> pure ()

  -- Recursively do it for the subDirs too
  mapM_ writeDirtyInfoRecursive dir.directorySubDirs
  where
    write :: (FSWrite m, Logger m) => Path Rel File -> BS.ByteString -> m ()
    write name file = do
      -- Make a backup if a file already exists, do this without parsing it, as it might (have) fail(ed) parsing
      let path = dir.directoryPath </> name
      case addExtension ".backup" path of
        Just backupPath -> renameFileSafe path backupPath
        Nothing -> logStr $ "Failed to make backup file name: " ++ toFilePath path

      -- Write the file to disk
      writeFileBS path file

-- Helpers

-- | Partition an array in 3. It'll try the first function first, then second, if both fail it'll be put in the 3th list for `other`.
partition3 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition3 p1 p2 arr = partition3' arr ([], [], [])
  where
    partition3' [] acc = acc
    partition3' (x : xs) (a1, a2, a3)
      | p1 x = partition3' xs (x : a1, a2, a3)
      | p2 x = partition3' xs (a1, x : a2, a3)
      | otherwise = partition3' xs (a1, a2, x : a3)

-- | Sorts the dirs by name, taking into account numbers properly
smartDirSort :: [Directory] -> [Directory]
smartDirSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceDirNameT a.directoryPath)
        (T.toLower $ niceDirNameT b.directoryPath)

-- | Sorts the dirs, taking into account numbers properly
smartFileSortBy :: (a -> Path x File) -> [a] -> [a]
smartFileSortBy getPath = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower . niceFileNameT $ getPath a)
        (T.toLower . niceFileNameT $ getPath b)
