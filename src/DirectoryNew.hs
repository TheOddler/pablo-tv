module DirectoryNew where

import Autodocodec (HasCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Control.Exception (IOException, SomeException, try)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Yaml qualified as Yaml
import DirectoryOld (DirectoryInfo)
import System.Directory
import System.FilePath (takeExtension, takeFileName, (</>))
import System.Posix (FileStatus, getFileStatus)
import Watched (WatchedFiles)

newtype RootDirectory = RootDirectory FilePath

getVideosDir :: IO RootDirectory
getVideosDir = do
  home <- getHomeDirectory
  let videoDirName = "Videos"
  pure $ RootDirectory $ home </> videoDirName

infoFileName :: FilePath
infoFileName = "info.yaml"

watchedFileName :: FilePath
watchedFileName = "watched.yaml"

-- | The extensions we consider video files.
-- Must include the `.` as that's what `takeExtension` gives us so easier to use that way.
videoExtensions :: [String]
videoExtensions = [".mp4", ".mkv", ".avi", ".webm"]

data VideoFile = VideoFile FilePath FileStatus

newtype OtherFile = OtherFile FilePath

-- | Extra info needed to manage the info.yaml and watched.yaml files.
-- This keeps info that we can use to know whether we need to save updated info to disk.
data SpecialFile a
  = FileDoesNotExist
  | FileRead a
  | -- | If we update the file in memory mark it as dirty so we know we need to write it to disk.
    FileDirty a
  | -- | If a file existed but parsing failed
    FileReadFail BS8.ByteString Yaml.ParseException
  | -- | If we got an error while trying to read the file
    FileReadError SomeException

data Directory = Directory
  { directoryPath :: FilePath,
    directoryInfo :: SpecialFile DirectoryInfo,
    directoryWatched :: SpecialFile WatchedFiles,
    directoryVideoFiles :: [VideoFile],
    directoryOtherFiles :: [OtherFile],
    directorySubDirs :: [Directory]
  }

readRootDirectory :: RootDirectory -> IO Directory
readRootDirectory (RootDirectory path) = readDirectory path

readDirectory :: FilePath -> IO Directory
readDirectory path = do
  subPaths <- listDirectory path
  -- Pre-process all the paths we founds
  let (videoRelPaths, dirInfoRelPaths, watchedRelPaths, otherRelPaths) =
        partition4
          -- This assumes `.yaml` is not considered a video extension, otherwise we'll never find the info or watched files
          (extensionIsOneOf videoExtensions)
          (fileNameIs infoFileName)
          (fileNameIs watchedFileName)
          subPaths
  let -- Make the paths absolute, and save them like that for easy reading
      videoPaths = (path </>) <$> videoRelPaths
      dirInfoPaths = (path </>) <$> dirInfoRelPaths
      watchedPaths = (path </>) <$> watchedRelPaths
      otherPaths = (path </>) <$> otherRelPaths

  -- Parse special files
  let mDirInfoPath = listToMaybe dirInfoPaths
  dirInfo <- maybe (pure FileDoesNotExist) readSpecialFile mDirInfoPath
  let mWatchedPath = listToMaybe watchedPaths
  watched <- maybe (pure FileDoesNotExist) readSpecialFile mWatchedPath

  -- Parse the video files
  let readVideoFile p = VideoFile p <$> getFileStatus p
  videoFiles <- mapM readVideoFile videoPaths

  -- Find all the subDirs and parse those too.
  -- TODO: Should we do better error handling here?
  -- Essentially we abuse the "does not exist" error to separate dirs and files.
  (readDirResult :: [Either IOException Directory]) <- mapM (try . readDirectory) otherPaths
  let (others', subDirs') = partitionFst $ zip readDirResult otherPaths
  let subDirs = fst <$> subDirs'
  let others = OtherFile . snd <$> others'

  -- Finished
  pure
    Directory
      { directoryPath = path,
        directoryInfo = dirInfo,
        directoryWatched = watched,
        directoryVideoFiles = sortOn (\(VideoFile p _) -> p) videoFiles,
        directorySubDirs = sortOn directoryPath subDirs,
        directoryOtherFiles = sortOn (\(OtherFile p) -> p) others
      }

-- Helpers
fileNameIs :: String -> FilePath -> Bool
fileNameIs n = (== n) . takeFileName

extensionIsOneOf :: [String] -> FilePath -> Bool
extensionIsOneOf exts path =
  takeExtension path `elem` exts

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

partitionFst :: [(Either a b, c)] -> ([(a, c)], [(b, c)])
partitionFst arr = partitionFst' arr ([], [])
  where
    partitionFst' [] acc = acc
    partitionFst' ((Left a, c) : rest) (acs, bcs) = partitionFst' rest ((a, c) : acs, bcs)
    partitionFst' ((Right b, c) : rest) (acs, bcs) = partitionFst' rest (acs, (b, c) : bcs)

readSpecialFile :: (HasCodec a) => FilePath -> IO (SpecialFile a)
readSpecialFile path = do
  (contentOrErr :: Either SomeException BS8.ByteString) <-
    try $ BS8.readFile path
  pure $ case contentOrErr of
    Left err -> FileReadError err
    Right content ->
      case eitherDecodeYamlViaCodec content of
        Right info -> FileRead info
        Left err -> FileReadFail content err
