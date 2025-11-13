module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, eitherDecodeFileStrict, encodeFile, genericParseJSON, genericToEncoding)
import Data.Foldable (foldrM)
import Data.HashSet qualified as Set
import Data.List (intercalate, sortBy, (\\))
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.IO (catchAny)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Read (Read (..))
import GHC.Utils.Exception (displayException, tryIO)
import Logging (LogLevel (..), Logger, putLog)
import Samba (SmbServer (..), SmbShare (..))
import Samba qualified
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getHomeDirectory, getModificationTime, getXdgDirectory, listDirectory)
import System.FilePath (dropTrailingPathSeparator, takeBaseName, takeExtension, (</>))
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (failE, impossible, logDuration, ourAesonOptions, safeMinimumOn)
import Yesod (MonadIO (..), PathPiece (..))

newtype DirectoryName = DirectoryName {unDirectoryName :: Text}
  deriving (Eq, Ord, Generic, PathPiece)

instance ToJSON DirectoryName where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON DirectoryName where
  parseJSON = genericParseJSON ourAesonOptions

instance ToJSONKey DirectoryName

instance FromJSONKey DirectoryName

instance Show DirectoryName where
  showsPrec p (DirectoryName a) = showsPrec p a

instance Read DirectoryName where
  readPrec = do
    str <- readPrec
    when ('/' `elem` str) $ fail "DirectoryName cannot contain '/'"
    pure $ DirectoryName $ T.pack str

newtype VideoFileName = VideoFileName {unVideoFileName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VideoFileName where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON VideoFileName where
  parseJSON = genericParseJSON ourAesonOptions

instance ToJSONKey VideoFileName

instance FromJSONKey VideoFileName

newtype ImageFileName = ImageFileName Text
  deriving (Eq, Generic)

instance ToJSON ImageFileName where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON ImageFileName where
  parseJSON = genericParseJSON ourAesonOptions

newtype IgnoredFileName = IgnoredFileName Text

data VideoFileData = VideoFileData
  { videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }
  deriving (Generic)

instance ToJSON VideoFileData where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON VideoFileData where
  parseJSON = genericParseJSON ourAesonOptions

data DirectoryData = DirectoryData
  { directoryImage :: Maybe ImageFileName,
    directorySubDirs :: Map.Map DirectoryName DirectoryData,
    directoryVideoFiles :: Map.Map VideoFileName VideoFileData
  }
  deriving (Generic)

instance ToJSON DirectoryData where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON DirectoryData where
  parseJSON = genericParseJSON ourAesonOptions

data RootDirectoryLocation
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootLocalVideos
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON RootDirectoryLocation where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON RootDirectoryLocation where
  parseJSON = genericParseJSON ourAesonOptions

instance ToJSONKey RootDirectoryLocation

instance FromJSONKey RootDirectoryLocation

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

data RootDirectoryData = RootDirectoryData
  { rootDirectorySubDirs :: Map.Map DirectoryName DirectoryData,
    rootDirectoryVideoFiles :: Map.Map VideoFileName VideoFileData
  }
  deriving (Generic)

instance ToJSON RootDirectoryData where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON RootDirectoryData where
  parseJSON = genericParseJSON ourAesonOptions

rootDirectoryLocationName :: RootDirectoryLocation -> DirectoryName
rootDirectoryLocationName l = DirectoryName $ toPathPiece l

rootDirectoryAsDirectory :: RootDirectoryData -> DirectoryData
rootDirectoryAsDirectory root =
  DirectoryData
    { directoryImage = Nothing,
      directorySubDirs = root.rootDirectorySubDirs,
      directoryVideoFiles = root.rootDirectoryVideoFiles
    }

rootDirectoryPath :: RootDirectoryLocation -> DirectoryPath
rootDirectoryPath r = DirectoryPath r []

type RootDirectories = Map.Map RootDirectoryLocation RootDirectoryData

data DirectoryPath = DirectoryPath
  { directoryPathRoot :: RootDirectoryLocation,
    directoryPathNames :: [DirectoryName]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DirectoryPath where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON DirectoryPath where
  parseJSON = genericParseJSON ourAesonOptions

addSubDir :: DirectoryPath -> DirectoryName -> DirectoryPath
addSubDir (DirectoryPath root names) newName = DirectoryPath root $ names ++ [newName]

directoryPathToAbsPath :: (MonadIO m) => DirectoryPath -> m FilePath
directoryPathToAbsPath (DirectoryPath root dirNames) = do
  rootAbsPath <- case root of
    RootSamba srv shr -> Samba.mkMountPath srv shr
    RootLocalVideos -> do
      home <- liftIO getHomeDirectory
      pure $ home ++ "/Videos"
  pure $ intercalate "/" $ rootAbsPath : (T.unpack . unDirectoryName <$> dirNames)

data VideoFilePath = VideoFilePath
  { videoFilePathRoot :: RootDirectoryLocation,
    videoFilePathNames :: [DirectoryName],
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq, Generic)

instance ToJSON VideoFilePath where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON VideoFilePath where
  parseJSON = genericParseJSON ourAesonOptions

videoFilePath :: DirectoryPath -> VideoFileName -> VideoFilePath
videoFilePath dir videoName =
  VideoFilePath
    { videoFilePathRoot = dir.directoryPathRoot,
      videoFilePathNames = dir.directoryPathNames,
      videoFilePathName = videoName
    }

videoFilePathToAbsPath :: VideoFilePath -> IO FilePath
videoFilePathToAbsPath (VideoFilePath root dirNames fileName) = do
  dirAbsPath <- directoryPathToAbsPath $ DirectoryPath root dirNames
  pure $ dirAbsPath ++ "/" ++ T.unpack (unVideoFileName fileName)

getDirectoryAtPath :: RootDirectories -> DirectoryPath -> Maybe DirectoryData
getDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) = do
  root <- Map.lookup wantedRoot roots
  innerGet wantedDirNames $ rootDirectoryAsDirectory root
  where
    innerGet :: [DirectoryName] -> DirectoryData -> Maybe DirectoryData
    innerGet [] _ = Nothing
    innerGet [name] dir =
      Map.lookup name dir.directorySubDirs
    innerGet (name : rest) dir = do
      subDir <- Map.lookup name dir.directorySubDirs
      innerGet rest subDir

-- | Will silently do nothing if the path isn't found.
updateDirectoryAtPath :: RootDirectories -> DirectoryPath -> (DirectoryData -> DirectoryData) -> RootDirectories
updateDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) updateFunc =
  Map.adjust
    ( let dirBackToRoot dir =
            RootDirectoryData
              { rootDirectorySubDirs = dir.directorySubDirs,
                rootDirectoryVideoFiles = dir.directoryVideoFiles
              }
       in dirBackToRoot . innerUpdate wantedDirNames . rootDirectoryAsDirectory
    )
    wantedRoot
    roots
  where
    innerUpdate :: [DirectoryName] -> DirectoryData -> DirectoryData
    innerUpdate [] dirData = updateFunc dirData
    innerUpdate (name : ns) dirData =
      dirData
        { directorySubDirs = Map.adjust (innerUpdate ns) name dirData.directorySubDirs
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
  "" -> LikelyIgnored $ IgnoredFileName $ T.pack p
  '.' : _ -> LikelyIgnored $ IgnoredFileName $ T.pack p
  _ -> case takeExtension p of
    ext | isVideoFileExt ext -> LikelyVideoFile $ VideoFileName $ T.pack p
    ext | isImageFileExt ext -> LikelyImageFile $ ImageFileName $ T.pack p
    ext | isCommonFileExt ext -> LikelyIgnored $ IgnoredFileName $ T.pack p
    _ -> LikelyDir $ DirectoryName $ T.pack p

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
  case lower $ takeBaseName $ T.unpack fileName of
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
      subDirsSeasonIndicators =
        any (isJust . seasonFromDir) $ Map.keys dirData.directorySubDirs
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

data DirectoryUpdateResult
  = DirectoryUnchanged
  | DirectoryChanged DirectoryData
  | DirectoryNotADirectory
  | -- | A special case, it's possible for a root not to be found, if for example the samba share isn't mounted.
    DirectoryNotFoundRoot

updateRootDirectoriesFromDisk :: (MonadIO m, MonadThrow m, Logger m) => RootDirectories -> m RootDirectories
updateRootDirectoriesFromDisk rootDirs =
  flip Map.traverseWithKey rootDirs $ \rootLocation rootDirData -> do
    let path = rootDirectoryPath rootLocation
    let rootAsDir = rootDirectoryAsDirectory rootDirData
    result <- updateDirectoryFromDisk path rootAsDir
    case result of
      DirectoryChanged updated ->
        pure
          RootDirectoryData
            { rootDirectorySubDirs = updated.directorySubDirs,
              rootDirectoryVideoFiles = updated.directoryVideoFiles
            }
      DirectoryUnchanged -> pure rootDirData
      DirectoryNotADirectory -> do
        let name = rootDirectoryLocationName rootLocation
        putLog Warning $
          unwords
            [ "Root directory",
              T.unpack name.unDirectoryName,
              "wasn't a directory somehow?"
            ]
        pure rootDirData
      DirectoryNotFoundRoot ->
        -- Don't remove the root dir, this should only be done manually.
        pure rootDirData

memoryFileName :: FilePath
memoryFileName = "pablo-tv.json"

getMemoryFileDir :: (MonadIO m) => m FilePath
getMemoryFileDir = liftIO $ getXdgDirectory XdgData ""

-- | Writes the known disks to a json file on disk, so we can read it on startup next time.
saveRootsToDisk :: (MonadIO m, Logger m) => RootDirectories -> m ()
saveRootsToDisk roots = logDuration "Saved roots to disk" $ do
  memoryDir <- getMemoryFileDir
  liftIO $ do
    createDirectoryIfMissing True memoryDir
    encodeFile (memoryDir </> memoryFileName) roots

loadRootsFromDisk :: (Logger m, MonadIO m) => m (Maybe RootDirectories)
loadRootsFromDisk = do
  memoryDir <- getMemoryFileDir
  let memoryFile = memoryDir </> memoryFileName
  let safeDecode =
        eitherDecodeFileStrict memoryFile
          `catchAny` \e -> pure $ Left $ displayException e
  rootsOrErr <- logDuration "Loaded roots from disk" $ liftIO safeDecode
  case rootsOrErr of
    Right roots -> pure $ Just roots
    Left err -> do
      putLog Error $ "Failed loading roots from disk: " ++ err
      pure Nothing

-- | Updates the directory (at given path) with new data from disk.
-- It will potentially remove or add video files and sub-directories.
-- It leaves watched or added information for files in tact.
updateDirectoryFromDisk :: (MonadIO m, MonadThrow m, Logger m) => DirectoryPath -> DirectoryData -> m DirectoryUpdateResult
updateDirectoryFromDisk dirPath dir = do
  absDirPath <- directoryPathToAbsPath dirPath
  (namesOrErr :: Either IOError [FilePath]) <- liftIO $ tryIO $ listDirectory absDirPath
  case namesOrErr of
    Left err | err.ioe_type == InappropriateType -> do
      putLog Warning $ "What we thought was a directory turned out not to be: " ++ absDirPath
      pure DirectoryNotADirectory
    Left err | err.ioe_type == NoSuchThing && null dirPath.directoryPathNames -> do
      putLog Warning $ "Root folder not found: " ++ absDirPath
      pure DirectoryNotFoundRoot
    Left err -> do
      failE ("Updating directory " ++ absDirPath) err
    Right names -> do
      let typeGuesses = guessType <$> names
      let (subDirGuesses, videoGuesses, imageGuesses, _ignored) = partitionPathTypes typeGuesses
      -- Recursively check for directory updates
      subDirUpdates <- forM subDirGuesses $ \dirName -> do
        let knownDir = Map.lookup dirName dir.directorySubDirs
        let newDir = DirectoryData Nothing Map.empty Map.empty
        let fullPath = addSubDir dirPath dirName
        upd <- updateDirectoryFromDisk fullPath $ knownDir `orElse` newDir
        pure (dirName, upd)
      (unchanged, changed, notDirs) <-
        foldrM
          ( \(name, upd) (un, ch, nd) -> case upd of
              DirectoryUnchanged -> pure (name : un, ch, nd)
              DirectoryChanged d -> pure (un, (name, d) : ch, nd)
              DirectoryNotADirectory -> pure (un, ch, name : nd)
              DirectoryNotFoundRoot -> impossible "This should only happen on roots, and thus should be impossible here."
          )
          ([], [], [])
          subDirUpdates
      let actualSubDirs = subDirGuesses \\ notDirs
      let noLongerExist = Map.keys dir.directorySubDirs \\ actualSubDirs
      let updatedSubDirs :: Maybe (Map.Map DirectoryName DirectoryData)
          updatedSubDirs = case (unchanged, changed, noLongerExist) of
            (_, [], []) -> Nothing -- Nothing indicates no changes were found
            _ -> do
              -- Step 1: Remove dirs that that no longer exist
              let step1 = foldr Map.delete dir.directorySubDirs noLongerExist
              -- Step 2: Update the updated dirs (insert overwrites)
              let step2 = foldr (uncurry Map.insert) step1 changed
              Just step2

      -- Check if there are any new files or remove files
      -- Files we already knew about, we won't check again so we don't do unneeded IO
      let knownVideoNames = Map.keys dir.directoryVideoFiles
      let newVideoNames = videoGuesses \\ knownVideoNames
      let removedVideoNames = knownVideoNames \\ videoGuesses
      updatedVideoFiles <- case (newVideoNames, removedVideoNames) of
        ([], []) -> pure Nothing
        _ -> do
          -- Step 1: Remove files that no longer exist
          let step1 = foldr Map.delete dir.directoryVideoFiles removedVideoNames
          -- Step 2: Add new videos
          newVideos <- forM newVideoNames $ \newVideoName -> do
            let fullFilePath = absDirPath ++ "/" ++ T.unpack newVideoName.unVideoFileName
            modTime <- liftIO $ getModificationTime fullFilePath
            pure
              ( newVideoName,
                VideoFileData
                  { videoFileAdded = modTime,
                    videoFileWatched = Nothing
                  }
              )
          let step2 = foldr (uncurry Map.insert) step1 newVideos
          pure $ Just step2

      -- We only care about the best image, other images ignore, even if there are new ones that aren't the best
      let bestImage = bestImageFile imageGuesses
      -- TODO: If we don't have an image, see if we can download one using TVDB. Or perhaps we do that in a separate function

      pure $ case (updatedSubDirs, updatedVideoFiles, bestImage) of
        (Nothing, Nothing, _) | bestImage == dir.directoryImage -> DirectoryUnchanged
        _ ->
          DirectoryChanged $
            DirectoryData
              { directoryImage = bestImage,
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

foldFilesDataRecur :: forall a. (VideoFileData -> a -> a) -> a -> DirectoryData -> a
foldFilesDataRecur updateAgg agg dir = loop agg [dir]
  where
    loop :: a -> [DirectoryData] -> a
    loop a [] = a
    loop a (dirTodo : restDirs) = do
      let newA = foldr updateAgg a dirTodo.directoryVideoFiles
          subDirs = Map.elems dirTodo.directorySubDirs
      loop newA $ restDirs ++ subDirs

getSubDirAggInfo :: DirectoryPath -> DirectoryData -> [AggDirInfo]
getSubDirAggInfo dirPath dir = do
  let epoch = posixSecondsToUTCTime 0
  flip map (Map.toList dir.directorySubDirs) $ \(subDirName, subDirData) ->
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

readInt :: Text -> Maybe Int
readInt = readMaybe . T.unpack

tryRegex :: Text -> ([Text] -> Maybe a) -> Text -> Maybe a
tryRegex source resultParser regex =
  let res :: (Text, Text, Text, [Text])
      res = source =~ regex
      (_, _, _, matches) = res
   in resultParser matches

expect1Int :: [Text] -> Maybe Int
expect1Int = \case
  [a] ->
    readInt a
  _ -> Nothing

expect2Ints :: [Text] -> Maybe (Int, Int)
expect2Ints = \case
  [a, b] ->
    (,) <$> readInt a <*> readInt b
  _ -> Nothing

expect3Ints :: [Text] -> Maybe (Int, Int, Int)
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
seasonFromFiles fileNames =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
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
niceDirNameT (DirectoryName dir) = T.pack $ dropTrailingPathSeparator $ T.unpack dir

niceFileNameT :: VideoFileName -> Text
niceFileNameT (VideoFileName file) =
  T.replace "." " " $ T.pack $ takeBaseName $ T.unpack file

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
