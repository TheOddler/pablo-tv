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
import Data.Map qualified as Map
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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

data VideoFileData = VideoFileData
  { videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }

data DirectoryData = DirectoryData
  { directoryVideoFiles :: Map.Map VideoFileName VideoFileData,
    directoryImage :: Maybe ImageFileName,
    directorySubDirs :: Map.Map DirectoryName DirectoryData
  }

data RootDirectoryType
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootLocalVideos
  deriving (Eq, Ord, Show, Read)

instance PathPiece RootDirectoryType where
  toPathPiece :: RootDirectoryType -> Text
  toPathPiece = \case
    RootSamba srv shr -> T.pack $ srv.unSmbServer ++ "/" ++ shr.unSmbShare
    RootLocalVideos -> "Videos"

  fromPathPiece :: Text -> Maybe RootDirectoryType
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
  { rootDirectoryType :: RootDirectoryType,
    rootDirectoryData :: DirectoryData
  }

type RootDirectories = [RootDirectory]

rootDirectoryPath :: RootDirectory -> DirectoryPath
rootDirectoryPath root = DirectoryPath root.rootDirectoryType []

rootDirectoryAsDirectory :: RootDirectory -> Directory
rootDirectoryAsDirectory root = Directory (rootDirectoryPath root) root.rootDirectoryData

data DirectoryPath = DirectoryPath
  { directoryPathRoot :: RootDirectoryType,
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
  { videoFilePathDir :: DirectoryPath,
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq)

videoFilePathToAbsPath :: VideoFilePath -> IO FilePath
videoFilePathToAbsPath (VideoFilePath dirPath fileName) = do
  dirAbsPath <- directoryPathToAbsPath dirPath
  pure $ dirAbsPath ++ "/" ++ unVideoFileName fileName

getDirectoryData :: RootDirectories -> DirectoryPath -> Maybe DirectoryData
getDirectoryData roots (DirectoryPath wantedRoot wantedDirNames) = do
  root <- find (\r -> r.rootDirectoryType == wantedRoot) roots
  innerGet wantedDirNames root.rootDirectoryData
  where
    innerGet :: [DirectoryName] -> DirectoryData -> Maybe DirectoryData
    innerGet [] _ = Nothing
    innerGet [name] dirData = Map.lookup name dirData.directorySubDirs
    innerGet (name : rest) dirData = do
      nextDir <- Map.lookup name dirData.directorySubDirs
      innerGet rest nextDir

-- | Will silently do nothing if the path isn't found.
-- TODO: This function is very broken, it seems to just overwrite the root dir's data
updateDirectory :: RootDirectories -> DirectoryPath -> (DirectoryData -> DirectoryData) -> RootDirectories
updateDirectory currentRoots (DirectoryPath wantedRoot wantedDirNames) updateFunc =
  flip map currentRoots $ \currentRoot -> do
    if currentRoot.rootDirectoryType /= wantedRoot
      then currentRoot -- Leave this one alone
      else
        currentRoot
          { rootDirectoryData = innerUpdate wantedDirNames currentRoot.rootDirectoryData
          }
  where
    innerUpdate :: [DirectoryName] -> DirectoryData -> DirectoryData
    innerUpdate [] d = updateFunc d
    innerUpdate (n : ns) d = case Map.lookup n d.directorySubDirs of
      Nothing -> d
      Just subDir -> innerUpdate ns subDir

data Directory = Directory DirectoryPath DirectoryData

data VideoFile = VideoFile
  { videoFilePath :: VideoFilePath,
    videoFileData :: VideoFileData
  }

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

guessDirectoryKind :: DirectoryName -> DirectoryData -> DirectoryKindGuess
guessDirectoryKind dirName dirData =
  let title = titleFromDir dirName
      dirSeasonIndicator = isJust $ seasonFromDir dirName
      subDirsSeasonIndicators = any (isJust . seasonFromDir) $ Map.keys dirData.directorySubDirs
      hasSubDirs = not $ null dirData.directorySubDirs
      filesSeasonIndicator = isJust $ seasonFromFiles $ Map.keys dirData.directoryVideoFiles
      hasMovieFiles = not $ null dirData.directoryVideoFiles
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

data DirectoryCheckResult
  = DirectoryUnchanged
  | DirectoryChanged DirectoryData
  | DirectoryNotADirectory

-- | Checks the directory with what it finds on disk, and returns the updates version, or Nothing if it's already the same as on disk.
checkDirectory :: Directory -> IO DirectoryCheckResult
checkDirectory (Directory dirPath dirData) = do
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
        let knownData = Map.lookup dirName dirData.directorySubDirs
        let emptyData = DirectoryData Map.empty Nothing Map.empty
        let fullPath = addSubDir dirPath dirName
        upd <- checkDirectory (Directory fullPath $ knownData `orElse` emptyData)
        pure (dirName, upd)
      let (unchanged, changed, notDirs) =
            foldr
              ( \(name, upd) (un, ch, nd) -> case upd of
                  DirectoryUnchanged -> (name : un, ch, nd)
                  DirectoryChanged dir -> (un, (name, dir) : ch, nd)
                  DirectoryNotADirectory -> (un, ch, name : nd)
              )
              ([], [], [])
              subDirUpdates
      let actualSubDirs = subDirGuesses \\ notDirs
      let noLongerExist = Map.keys dirData.directorySubDirs \\ actualSubDirs
      let updatedSubDirs = case (unchanged, changed, noLongerExist) of
            (_, [], []) -> Nothing -- Nothing indicates no changes were found
            _ -> do
              let removedNonExistant = foldr Map.delete dirData.directorySubDirs noLongerExist
              let updated = foldr (uncurry Map.insert) removedNonExistant changed
              Just updated

      -- Check if there are any new files or remove files
      -- Files we already knew about, we won't check again so we don't do unneeded IO
      let newVideoNames = videoGuesses \\ Map.keys dirData.directoryVideoFiles
      let removedVideoNames = Map.keys dirData.directoryVideoFiles \\ videoGuesses
      updatedVideoFiles <- case (newVideoNames, removedVideoNames) of
        ([], []) -> pure Nothing
        _ -> do
          let removedVideos = foldr Map.delete dirData.directoryVideoFiles removedVideoNames
          newVideos <- forM newVideoNames $ \newVideoName -> do
            let fullFilePath = absDirPath ++ "/" ++ unVideoFileName newVideoName
            modTime <- getModificationTime fullFilePath
            pure (newVideoName, VideoFileData modTime Nothing)
          let withNewVideos = foldr (uncurry Map.insert) removedVideos newVideos
          pure $ Just withNewVideos

      -- We only care about the best image, other images ignore, even if there are new ones that aren't the best
      let bestImage = bestImageFile imageGuesses
      -- TODO: If we don't have an image, see if we can download one using TVDB

      pure $ case (updatedSubDirs, updatedVideoFiles, bestImage) of
        (Nothing, Nothing, _) | bestImage == dirData.directoryImage -> DirectoryUnchanged
        _ ->
          DirectoryChanged $
            DirectoryData
              { directoryVideoFiles = updatedVideoFiles `orElse` dirData.directoryVideoFiles,
                directoryImage = bestImage,
                directorySubDirs = updatedSubDirs `orElse` dirData.directorySubDirs
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

foldFilesDataRecur :: (VideoFileData -> a -> a) -> a -> DirectoryData -> a
foldFilesDataRecur updateAgg agg dir = loop agg [dir]
  where
    loop a [] = a
    loop a (dirTodo : restDirs) = do
      let newA = foldr updateAgg a dirTodo.directoryVideoFiles
          subDirs = Map.elems dirTodo.directorySubDirs
      loop newA $ restDirs ++ subDirs

getSubDirAggInfo :: Directory -> [AggDirInfo]
getSubDirAggInfo (Directory dirPath dirData) = do
  let epoch = posixSecondsToUTCTime 0
  flip map (Map.assocs dirData.directorySubDirs) $ \(subDirName, subDirData) ->
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
          subDirName
          (addSubDir dirPath subDirName)
          epoch
          epoch
          0
          0
      )
      subDirData

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

seasonFromFiles :: [VideoFileName] -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ NE.group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

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
$(deriveJSON defaultOptions ''RootDirectoryType)
$(deriveJSON defaultOptions ''DirectoryPath)
$(deriveJSON defaultOptions ''VideoFileName)
$(deriveJSON defaultOptions ''VideoFilePath)
