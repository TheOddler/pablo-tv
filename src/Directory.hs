{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.HashSet qualified as Set
import Data.List (find, intercalate, sortBy, (\\))
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as VectorAlgs
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Read (Read (..))
import GHC.Utils.Exception (displayException, tryIO)
import Logging (LogLevel (..), putLog)
import Samba (SmbServer (..), SmbShare (..))
import Samba qualified
import System.Directory (getHomeDirectory, getModificationTime, listDirectory)
import System.FilePath (dropTrailingPathSeparator, takeBaseName, takeExtension)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (safeMinimumOn)
import Yesod (PathPiece (..))

newtype DirectoryName = DirectoryName {unDirectoryName :: String}
  deriving (Eq, Ord, Generic, PathPiece)

instance Show DirectoryName where
  showsPrec p (DirectoryName a) = showsPrec p a

instance Read DirectoryName where
  readPrec = do
    str <- readPrec
    when ('/' `elem` str) $ fail "DirectoryName cannot contain '/'"
    pure $ DirectoryName str

newtype VideoFileName = VideoFileName {unVideoFileName :: String}
  deriving (Show, Eq, Ord)

newtype ImageFileName = ImageFileName String
  deriving (Eq)

newtype IgnoredFileName = IgnoredFileName String

data VideoFile = VideoFile
  { videoFileName :: VideoFileName,
    videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }

data Directory = Directory
  { directoryName :: DirectoryName,
    directoryImage :: Maybe ImageFileName,
    directorySubDirs :: Vector.Vector Directory,
    directoryVideoFiles :: Vector.Vector VideoFile
  }

data RootDirectoryLocation
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootLocalVideos
  deriving (Eq, Ord, Show, Read)

instance PathPiece RootDirectoryLocation where
  toPathPiece :: RootDirectoryLocation -> Text
  toPathPiece = \case
    RootSamba srv shr -> T.pack $ srv.unSmbServer ++ "/" ++ shr.unSmbShare
    RootLocalVideos -> "Videos"

  fromPathPiece :: Text -> Maybe RootDirectoryLocation
  fromPathPiece t =
    if t == "Videos"
      then Just RootLocalVideos
      else case T.split (== '/') t of
        [srv, shr] ->
          Just $
            RootSamba
              (SmbServer $ T.unpack srv)
              (SmbShare $ T.unpack shr)
        _ -> Nothing

data RootDirectory = RootDirectory
  { rootDirectoryLocation :: RootDirectoryLocation,
    rootDirectoryDirs :: Vector.Vector Directory,
    rootDirectoryVideoFiles :: Vector.Vector VideoFile
  }

rootDirectoryLocationName :: RootDirectoryLocation -> DirectoryName
rootDirectoryLocationName l = DirectoryName $ T.unpack $ toPathPiece l

rootDirectoryName :: RootDirectory -> DirectoryName
rootDirectoryName root = rootDirectoryLocationName root.rootDirectoryLocation

rootDirectoryAsDirectory :: RootDirectory -> Directory
rootDirectoryAsDirectory root =
  Directory
    { directoryName = rootDirectoryName root,
      directoryImage = Nothing,
      directorySubDirs = root.rootDirectoryDirs,
      directoryVideoFiles = root.rootDirectoryVideoFiles
    }

rootDirectoryPath :: RootDirectory -> DirectoryPath
rootDirectoryPath r = DirectoryPath r.rootDirectoryLocation []

type RootDirectories = [RootDirectory]

data DirectoryPath = DirectoryPath
  { directoryPathRoot :: RootDirectoryLocation,
    directoryPathNames :: [DirectoryName]
  }
  deriving (Show, Eq)

addSubDir :: DirectoryPath -> DirectoryName -> DirectoryPath
addSubDir (DirectoryPath root names) newName = DirectoryPath root $ names ++ [newName]

directoryPathToAbsPath :: DirectoryPath -> IO FilePath
directoryPathToAbsPath (DirectoryPath root dirNames) = do
  rootAbsPath <- case root of
    RootSamba srv shr -> Samba.mkMountPath srv shr
    RootLocalVideos -> do
      home <- getHomeDirectory
      pure $ home ++ "/Videos"
  pure $ intercalate "/" $ rootAbsPath : (unDirectoryName <$> dirNames)

data VideoFilePath = VideoFilePath
  { videoFilePathRoot :: RootDirectoryLocation,
    videoFilePathNames :: [DirectoryName],
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq)

videoFilePath :: DirectoryPath -> VideoFile -> VideoFilePath
videoFilePath dir video =
  VideoFilePath
    { videoFilePathRoot = dir.directoryPathRoot,
      videoFilePathNames = dir.directoryPathNames,
      videoFilePathName = video.videoFileName
    }

videoFilePathToAbsPath :: VideoFilePath -> IO FilePath
videoFilePathToAbsPath (VideoFilePath root dirNames fileName) = do
  dirAbsPath <- directoryPathToAbsPath $ DirectoryPath root dirNames
  pure $ dirAbsPath ++ "/" ++ unVideoFileName fileName

getDirectoryAtPath :: RootDirectories -> DirectoryPath -> Maybe Directory
getDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) = do
  root <- find (\r -> r.rootDirectoryLocation == wantedRoot) roots
  innerGet wantedDirNames $ rootDirectoryAsDirectory root
  where
    innerGet :: [DirectoryName] -> Directory -> Maybe Directory
    innerGet [] _ = Nothing
    innerGet [name] dir =
      Vector.find ((== name) . directoryName) dir.directorySubDirs
    innerGet (name : rest) dir = do
      subDir <- Vector.find ((== name) . directoryName) dir.directorySubDirs
      innerGet rest subDir

-- | Will silently do nothing if the path isn't found.
updateDirectoryAtPath :: RootDirectories -> DirectoryPath -> (Directory -> Directory) -> RootDirectories
updateDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) updateFunc =
  flip map roots $ \currentRoot -> do
    if currentRoot.rootDirectoryLocation == wantedRoot
      then
        currentRoot
          { rootDirectoryDirs =
              fmap (innerUpdate wantedDirNames) currentRoot.rootDirectoryDirs
          }
      else currentRoot -- Leave this one alone
  where
    innerUpdate :: [DirectoryName] -> Directory -> Directory
    innerUpdate [] d = d
    innerUpdate [n] d = if d.directoryName == n then updateFunc d else d
    innerUpdate (n : ns) d =
      if d.directoryName == n
        then
          d
            { directorySubDirs = fmap (innerUpdate ns) d.directorySubDirs
            }
        else d

-- | Reading the file info of a path is rather slow, so we want to minimise the ones we read it for.
-- So instead of reading it for every path, we first do a guess what the path is, and read it for dirs only.
-- There's also some paths we ignore, regardless of wether they are files or dirs.
data LikelyPathType
  = LikelyDir DirectoryName
  | LikelyVideoFile VideoFileName
  | LikelyImageFile ImageFileName
  | LikelyIgnored IgnoredFileName

-- | This expects a string that's the result of a `listDirectory` call
guessType :: String -> LikelyPathType
guessType p = case p of
  "" -> LikelyIgnored $ IgnoredFileName p
  '.' : _ -> LikelyIgnored $ IgnoredFileName p
  _ -> case takeExtension p of
    ext | isVideoFileExt ext -> LikelyVideoFile $ VideoFileName p
    ext | isImageFileExt ext -> LikelyImageFile $ ImageFileName p
    ext | isCommonFileExt ext -> LikelyIgnored $ IgnoredFileName p
    _ -> LikelyDir $ DirectoryName p

-- | Sometimes we guess something is a directory, but it turns out not to be.
-- In that case we ignore it.
dirToOther :: DirectoryName -> IgnoredFileName
dirToOther (DirectoryName n) = IgnoredFileName n

partitionPathTypes :: [LikelyPathType] -> ([DirectoryName], [VideoFileName], [ImageFileName], [IgnoredFileName])
partitionPathTypes =
  foldr
    ( \guess (dirs, vids, imgs, igns) -> case guess of
        LikelyDir d -> (d : dirs, vids, imgs, igns)
        LikelyVideoFile v -> (dirs, v : vids, imgs, igns)
        LikelyImageFile i -> (dirs, vids, i : imgs, igns)
        LikelyIgnored i -> (dirs, vids, imgs, i : igns)
    )
    ([], [], [], [])

bestImageFile :: [ImageFileName] -> Maybe ImageFileName
bestImageFile = safeMinimumOn $ \(ImageFileName fileName) ->
  case lower $ takeBaseName fileName of
    "poster" -> 0 :: Int
    _ -> 100

data DirectoryKindGuess
  = DirectoryKindMovie Text -- Best guess for the movie title
  | DirectoryKindSeries Text -- Best guess for the series' name
  | DirectoryKindSeriesSeason -- For now we don't need any extra info about seasons
  | DirectoryKindUnknown

guessDirectoryKind :: Directory -> DirectoryKindGuess
guessDirectoryKind dir =
  let title = titleFromDir dir.directoryName
      dirSeasonIndicator = isJust $ seasonFromDir dir.directoryName
      subDirsSeasonIndicators =
        any (isJust . seasonFromDir . directoryName) dir.directorySubDirs
      hasSubDirs = not $ null dir.directorySubDirs
      filesSeasonIndicator = isJust $ seasonFromFiles dir.directoryVideoFiles
      hasMovieFiles = not $ null dir.directoryVideoFiles
   in firstJusts
        [ if dirSeasonIndicator
            then Just DirectoryKindSeriesSeason
            else Nothing,
          if subDirsSeasonIndicators && not hasMovieFiles
            then Just $ DirectoryKindSeries title
            else Nothing,
          if filesSeasonIndicator && not hasSubDirs
            then Just $ DirectoryKindSeries title
            else Nothing,
          if not filesSeasonIndicator && not hasSubDirs
            then Just $ DirectoryKindMovie title
            else Nothing
        ]
        `orElse` DirectoryKindUnknown

data DirectoryUpdateResult
  = DirectoryUnchanged
  | DirectoryChanged Directory
  | DirectoryNotADirectory

updateRootDirectoriesFromDisk :: RootDirectories -> IO RootDirectories
updateRootDirectoriesFromDisk rootDirs =
  forM rootDirs $ \rootDir -> do
    let path = rootDirectoryPath rootDir
    let dir = rootDirectoryAsDirectory rootDir
    result <- updateDirectoryFromDisk path dir
    case result of
      DirectoryChanged updated ->
        pure
          RootDirectory
            { rootDirectoryLocation = rootDir.rootDirectoryLocation,
              rootDirectoryDirs = updated.directorySubDirs,
              rootDirectoryVideoFiles = updated.directoryVideoFiles
            }
      DirectoryUnchanged -> pure rootDir
      DirectoryNotADirectory -> do
        let name = rootDirectoryName rootDir
        putLog Warning $
          unwords
            [ "Root directory",
              name.unDirectoryName,
              "wasn't a directory somehow?"
            ]
        pure rootDir

-- | Updates the directory (at given path) with new data from disk.
-- It will potentially remove or add video files and sub-directories.
-- It leaves watched or added information for files in tact.
updateDirectoryFromDisk :: DirectoryPath -> Directory -> IO DirectoryUpdateResult
updateDirectoryFromDisk dirPath dir = do
  absDirPath <- directoryPathToAbsPath dirPath
  (namesOrErr :: Either IOError [FilePath]) <- tryIO $ listDirectory absDirPath
  case namesOrErr of
    Left err | err.ioe_type == InappropriateType -> do
      putLog Warning $ "Tried updating non-existant directory " ++ absDirPath
      pure DirectoryNotADirectory
    Left err -> do
      putLog Error $
        "IO error while trying to update directory "
          ++ absDirPath
          ++ ": "
          ++ displayException err
      throwIO err
    Right names -> do
      let typeGuesses = guessType <$> names
      let (subDirGuesses, videoGuesses, imageGuesses, _ignored) = partitionPathTypes typeGuesses
      -- Recursively check for directory updates
      subDirUpdates <- forM subDirGuesses $ \dirName -> do
        let knownDir = Vector.find ((== dirName) . directoryName) dir.directorySubDirs
        let newDir = Directory dirName Nothing Vector.empty Vector.empty
        let fullPath = addSubDir dirPath dirName
        upd <- updateDirectoryFromDisk fullPath $ knownDir `orElse` newDir
        pure (dirName, upd)
      let (unchanged, changed, notDirs) =
            foldr
              ( \(name, upd) (un, ch, nd) -> case upd of
                  DirectoryUnchanged -> (name : un, ch, nd)
                  DirectoryChanged d -> (un, (name, d) : ch, nd)
                  DirectoryNotADirectory -> (un, ch, name : nd)
              )
              ([], [], [])
              subDirUpdates
      let actualSubDirs = subDirGuesses \\ notDirs
      let noLongerExist =
            fmap directoryName (Vector.toList dir.directorySubDirs) \\ actualSubDirs
      let updatedSubDirs :: Maybe (Vector.Vector Directory)
          updatedSubDirs = case (unchanged, changed, noLongerExist) of
            (_, [], []) -> Nothing -- Nothing indicates no changes were found
            _ -> do
              -- Step 1: Remove dirs that that no longer exist or that were changed as we'll add their updated version back after
              let toRemove = noLongerExist ++ map fst changed
              let step1 = Vector.filter ((`notElem` toRemove) . directoryName) dir.directorySubDirs
              -- Step 2: Add the updates back
              let step2 = step1 <> Vector.fromList (map snd changed)
              -- Finally sort them nicely
              let nameSorter = naturalCompareBy $ unDirectoryName . directoryName
              Just $ Vector.modify (VectorAlgs.sortBy nameSorter) step2

      -- Check if there are any new files or remove files
      -- Files we already knew about, we won't check again so we don't do unneeded IO
      let knownVideoNames = fmap videoFileName (Vector.toList dir.directoryVideoFiles)
      let newVideoNames = videoGuesses \\ knownVideoNames
      let removedVideoNames = knownVideoNames \\ videoGuesses
      updatedVideoFiles <- case (newVideoNames, removedVideoNames) of
        ([], []) -> pure Nothing
        _ -> do
          -- Step 1: Remove files that no longer exist
          let step1 = Vector.filter ((`elem` removedVideoNames) . videoFileName) dir.directoryVideoFiles
          -- Step 2: Add new videos
          newVideos <- forM newVideoNames $ \newVideoName -> do
            let fullFilePath = absDirPath ++ "/" ++ unVideoFileName newVideoName
            modTime <- getModificationTime fullFilePath
            pure
              VideoFile
                { videoFileName = newVideoName,
                  videoFileAdded = modTime,
                  videoFileWatched = Nothing
                }
          let step2 = step1 <> Vector.fromList newVideos
          -- For files that we already knew about and that still exist, we do nothing so their watched and added info stays the same
          -- Finally we sort
          let nameSorter = naturalCompareBy $ unVideoFileName . videoFileName
          pure $ Just $ Vector.modify (VectorAlgs.sortBy nameSorter) step2

      -- We only care about the best image, other images ignore, even if there are new ones that aren't the best
      let bestImage = bestImageFile imageGuesses
      -- TODO: If we don't have an image, see if we can download one using TVDB. Or perhaps we do that in a separate function

      pure $ case (updatedSubDirs, updatedVideoFiles, bestImage) of
        (Nothing, Nothing, _) | bestImage == dir.directoryImage -> DirectoryUnchanged
        _ ->
          DirectoryChanged $
            Directory
              { directoryName = dir.directoryName,
                directoryImage = bestImage,
                directoryVideoFiles =
                  updatedVideoFiles `orElse` dir.directoryVideoFiles,
                directorySubDirs =
                  updatedSubDirs `orElse` dir.directorySubDirs
              }

-- Agg data
data AggDirInfo = AggDirInfo
  { aggDirName :: DirectoryName,
    aggDirPath :: DirectoryPath,
    aggDirLastModified :: UTCTime,
    aggDirLastWatched :: UTCTime,
    aggDirVideoFileCount :: Int,
    aggDirPlayedVideoFileCount :: Int
  }

foldFilesDataRecur :: forall a. (VideoFile -> a -> a) -> a -> Directory -> a
foldFilesDataRecur updateAgg agg dir = loop agg [dir]
  where
    loop :: a -> [Directory] -> a
    loop a [] = a
    loop a (dirTodo : restDirs) = do
      let newA = foldr updateAgg a dirTodo.directoryVideoFiles
          subDirs = Vector.toList dirTodo.directorySubDirs
      loop newA $ restDirs ++ subDirs

getSubDirAggInfo :: DirectoryPath -> Directory -> [AggDirInfo]
getSubDirAggInfo dirPath dir = do
  let epoch = posixSecondsToUTCTime 0
  flip map (Vector.toList dir.directorySubDirs) $ \subDir ->
    foldFilesDataRecur
      ( \videoFileData agg ->
          AggDirInfo
            agg.aggDirName
            agg.aggDirPath
            (max agg.aggDirLastModified videoFileData.videoFileAdded)
            ( case videoFileData.videoFileWatched of
                Nothing -> agg.aggDirLastWatched
                Just w -> max agg.aggDirLastWatched w
            )
            (agg.aggDirVideoFileCount + 1)
            ( case videoFileData.videoFileWatched of
                Nothing -> agg.aggDirPlayedVideoFileCount
                Just _ -> agg.aggDirPlayedVideoFileCount + 1
            )
      )
      ( AggDirInfo
          subDir.directoryName
          (addSubDir dirPath subDir.directoryName)
          epoch
          epoch
          0
          0
      )
      subDir

-- Some helpers
showUnique :: [String] -> String
showUnique els = intercalate ", " $ Set.toList $ Set.fromList els

isHiddenPath :: FilePath -> Bool
isHiddenPath = \case
  '.' : _ -> True
  _ -> False

readInt :: String -> Maybe Int
readInt = readMaybe

tryRegex :: String -> ([String] -> Maybe a) -> String -> Maybe a
tryRegex source resultParser regex =
  let res :: (String, String, String, [String])
      res = source =~ regex
      (_, _, _, matches) = res
   in resultParser matches

expect1Int :: [String] -> Maybe Int
expect1Int = \case
  [a] ->
    readInt a
  _ -> Nothing

expect2Ints :: [String] -> Maybe (Int, Int)
expect2Ints = \case
  [a, b] ->
    (,) <$> readInt a <*> readInt b
  _ -> Nothing

expect3Ints :: [String] -> Maybe (Int, Int, Int)
expect3Ints = \case
  [a, b, c] ->
    (,,) <$> readInt a <*> readInt b <*> readInt c
  _ -> Nothing

seasonFromDir :: DirectoryName -> Maybe Int
seasonFromDir (DirectoryName dir) =
  tryRegex dir expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: Vector.Vector VideoFile -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    fileNames = Vector.toList (videoFileName <$> files)
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ NE.group (mapMaybe seasonFromFile fileNames))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) fileNames

    seasonFromFile :: VideoFileName -> Maybe Int
    seasonFromFile (VideoFileName file) =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: VideoFileName -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile (VideoFileName file) =
  let double :: Maybe (Int, Int, Int)
      double =
        tryRegex file expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      seasonAndEp :: Maybe (Int, Int)
      seasonAndEp =
        tryRegex file expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex file expect2Ints "([0-9]+)[Xx]([0-9]+)"

      epOnly :: Maybe Int
      epOnly =
        tryRegex file expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex file expect1Int "[Aa]flevering ([0-9]+)"
   in case double of
        Just (s, a, b) -> (Just s, Just $ Right (a, b))
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ Left a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ Left a)
            Nothing -> (Nothing, Nothing)

titleFromDir :: DirectoryName -> Text
titleFromDir = T.strip . fst . T.breakOn "(" . niceDirNameT

niceDirNameT :: DirectoryName -> Text
niceDirNameT (DirectoryName dir) = T.pack $ dropTrailingPathSeparator dir

niceFileNameT :: VideoFileName -> Text
niceFileNameT (VideoFileName file) =
  T.replace "." " " $ T.pack $ takeBaseName file

isVideoFileExt :: String -> Bool
isVideoFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".avi",
        ".flv",
        ".m4v",
        ".mkv",
        ".mov",
        ".mp4",
        ".mpeg",
        ".mpg",
        ".webm",
        ".wmv"
      ]

isImageFileExt :: String -> Bool
isImageFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".bmp",
        ".gif",
        ".heic",
        ".heif",
        ".jpeg",
        ".jpg",
        ".png",
        ".svg",
        ".tif",
        ".tiff",
        ".webp"
      ]

-- | For non-video, non-image ext.
-- This is used to improve guesses on what are files and what are directories.
isCommonFileExt :: String -> Bool
isCommonFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".7z",
        ".aac",
        ".aes",
        ".apk",
        ".bat",
        ".csv",
        ".db",
        ".db3-shm",
        ".db3-wal",
        ".db3",
        ".dmg",
        ".doc",
        ".docx",
        ".exe",
        ".flac",
        ".gz",
        ".idx",
        ".img",
        ".iso",
        ".mp3",
        ".msi",
        ".odp",
        ".ods",
        ".odt",
        ".ogg",
        ".pdf",
        ".ppt",
        ".pptx",
        ".rar",
        ".rtf",
        ".sh",
        ".srt",
        ".sub",
        ".tar.gz",
        ".tar",
        ".txt",
        ".wav",
        ".xls",
        ".xlsx",
        ".yaml",
        ".zip"
      ]

-- | Compares taking into account numbers properly
naturalCompareBy :: (a -> String) -> a -> a -> Ordering
naturalCompareBy f a b = Natural.compare (lower $ f a) (lower $ f b)

-- | Sorts taking into account numbers properly
naturalSortBy :: (a -> String) -> [a] -> [a]
naturalSortBy f = sortBy $ naturalCompareBy f

$(deriveJSON defaultOptions ''DirectoryName)
$(deriveJSON defaultOptions ''RootDirectoryLocation)
$(deriveJSON defaultOptions ''DirectoryPath)
$(deriveJSON defaultOptions ''VideoFileName)
$(deriveJSON defaultOptions ''VideoFilePath)
