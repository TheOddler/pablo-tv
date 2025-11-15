{-# LANGUAGE DerivingStrategies #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey,
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    eitherDecodeFileStrict,
    encodeFile,
    genericParseJSON,
    genericToEncoding,
    withText,
  )
import Data.Aeson.Types (FromJSONKey (..), Parser, parseFail, toJSONKeyText)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.UTF8 qualified as BS
import Data.Foldable (foldrM)
import Data.HashSet qualified as Set
import Data.List (intercalate, sortBy, (\\))
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.IO (catchAny)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Utils.Exception (displayException, tryIO)
import ImageScraper (ImageSearchFailure (..), tryFindImage)
import Logging (LogLevel (..), Logger, putLog)
import Orphanage ()
import SafeConvert (bsToBase64Text)
import Samba (SmbServer (..), SmbShare (..))
import Samba qualified
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getHomeDirectory,
    getModificationTime,
    getXdgDirectory,
    listDirectory,
  )
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Blaze (ToMarkup)
import Text.Read (Read (..), readMaybe)
import Text.Regex.TDFA ((=~))
import Util
  ( failE,
    impossible,
    logDuration,
    ourAesonOptions,
    ourAesonOptionsPrefix,
    safeMinimumOn,
  )
import Util.TextWithoutSeparator
import Yesod (ContentType, MonadIO (..), PathPiece (..), ToContent, typeJpeg)

newtype DirectoryName = DirectoryName {unDirectoryName :: TextWithoutSeparator}
  deriving newtype (Show, Eq, Ord, Read, Unwrap Text, ToJSON, FromJSON, ToJSONKey, FromJSONKey, PathPiece, ToMarkup)

newtype VideoFileName = VideoFileName {unVideoFileName :: TextWithoutSeparator}
  deriving newtype (Show, Eq, Ord, Unwrap Text, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ImageFileName = ImageFileName TextWithoutSeparator
  deriving newtype (Eq, Show, Unwrap Text, ToJSON, FromJSON)

getImageContentType :: ImageFileName -> ContentType
getImageContentType (ImageFileName imgName) =
  case takeExtension $ T.unpack $ unwrap imgName of
    "" -> typeJpeg
    ext ->
      let cleanedExt = lower $
            case ext of
              '.' : e -> e
              e -> e
       in "image/" <> BS8.pack cleanedExt

-- | We assume this is a properly formatted content type, something like "image/jpg".
-- If it is not, we silently return some default
imageFileNameForContentType :: ContentType -> ImageFileName
imageFileNameForContentType ct = ImageFileName . removeSeparatorsFromText . T.pack $
  case BS.toString ct of
    'i' : 'm' : 'a' : 'g' : 'e' : '/' : ext -> "poster." ++ ext
    _ -> "poster.jpg"

newtype ImageFileData = ImageFileData {unImageFileData :: BS.ByteString}
  deriving newtype (ToContent, Eq, Show)

instance ToJSON ImageFileData where
  toJSON imgData = toJSON $ bsToBase64Text imgData.unImageFileData
  toEncoding imgData = toEncoding $ bsToBase64Text imgData.unImageFileData

instance FromJSON ImageFileData where
  parseJSON jsonValue = do
    t <- parseJSON jsonValue
    pure $ ImageFileData $ B64.decodeLenient $ TE.encodeUtf8 t

data VideoFileData = VideoFileData
  { videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show)

instance ToJSON VideoFileData where
  toEncoding = genericToEncoding $ ourAesonOptionsPrefix "videoFile"

instance FromJSON VideoFileData where
  parseJSON = genericParseJSON $ ourAesonOptionsPrefix "videoFile"

data DirectoryData = DirectoryData
  { directoryImage :: Maybe (ImageFileName, ImageFileData),
    directorySubDirs :: Map.Map DirectoryName DirectoryData,
    directoryVideoFiles :: Map.Map VideoFileName VideoFileData
  }
  deriving (Generic, Show, Eq)

instance ToJSON DirectoryData where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON DirectoryData where
  parseJSON = genericParseJSON ourAesonOptions

data RootDirectoryLocation
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootLocalVideos
  deriving (Eq, Ord, Generic)

unRootDirectoryLocation :: RootDirectoryLocation -> TextWithoutSeparator
unRootDirectoryLocation rd = removeSeparatorsFromText $ case rd of
  RootSamba srv shr ->
    T.pack $
      concat
        [ "smb-",
          srv.unSmbServer,
          "-",
          shr.unSmbShare
        ]
  RootLocalVideos -> "Videos"

rootDirectoryLocation :: (MonadFail m) => TextWithoutSeparator -> m RootDirectoryLocation
rootDirectoryLocation tws = do
  let t = unwrap tws
  if t == "Videos"
    then pure RootLocalVideos
    else case T.unpack <$> T.split (`elem` ['=', '-']) t of
      ["smbSrv", srv, "smbShr", shr] ->
        pure $
          RootSamba
            (SmbServer srv)
            (SmbShare shr)
      _ -> fail $ "Unknown root directory structure: " ++ T.unpack t

instance Read RootDirectoryLocation where
  readPrec = readPrec >>= rootDirectoryLocation

instance Show RootDirectoryLocation where
  show = T.unpack . unTextWithoutSeparator . unRootDirectoryLocation

instance ToJSON RootDirectoryLocation where
  toEncoding = toEncoding . unRootDirectoryLocation

instance FromJSON RootDirectoryLocation where
  parseJSON v = do
    (asText :: Text) <- parseJSON v
    parseRootDirectoryLocationFromText asText

parseRootDirectoryLocationFromText :: Text -> Parser RootDirectoryLocation
parseRootDirectoryLocationFromText text =
  textWithoutSeparator text >>= rootDirectoryLocation

instance ToJSONKey RootDirectoryLocation where
  toJSONKey = toJSONKeyText (unwrap . unRootDirectoryLocation)

instance FromJSONKey RootDirectoryLocation where
  fromJSONKey = FromJSONKeyTextParser parseRootDirectoryLocationFromText

instance PathPiece RootDirectoryLocation where
  toPathPiece :: RootDirectoryLocation -> Text
  toPathPiece = unwrap . unRootDirectoryLocation

  fromPathPiece :: Text -> Maybe RootDirectoryLocation
  fromPathPiece text = textWithoutSeparator text >>= rootDirectoryLocation

data RootDirectoryData = RootDirectoryData
  { rootDirectorySubDirs :: Map.Map DirectoryName DirectoryData,
    rootDirectoryVideoFiles :: Map.Map VideoFileName VideoFileData
  }
  deriving (Generic, Show, Eq)

instance ToJSON RootDirectoryData where
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON RootDirectoryData where
  parseJSON = genericParseJSON ourAesonOptions

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
  toEncoding (DirectoryPath root names) =
    toEncoding . unsplitSeparatedText $
      unRootDirectoryLocation root : (unDirectoryName <$> names)

instance FromJSON DirectoryPath where
  parseJSON = withText "DirectoryPath" $ \text ->
    case parseRootAndNames $ splitAtSeparatorNE text of
      Just (root, names) -> pure $ DirectoryPath root names
      Nothing -> parseFail $ "Couldn't parse DirectoryPath: " ++ T.unpack text

parseRootAndNames :: (MonadFail m) => NE.NonEmpty TextWithoutSeparator -> m (RootDirectoryLocation, [DirectoryName])
parseRootAndNames (mRoot NE.:| names) = do
  root <- rootDirectoryLocation mRoot
  pure (root, DirectoryName <$> names)

addSubDir :: DirectoryPath -> DirectoryName -> DirectoryPath
addSubDir (DirectoryPath root names) newName = DirectoryPath root $ names ++ [newName]

directoryPathToAbsPath :: (MonadIO m) => DirectoryPath -> m FilePath
directoryPathToAbsPath (DirectoryPath root dirNames) = do
  rootAbsPath <- case root of
    RootSamba srv shr -> Samba.mkMountPath srv shr
    RootLocalVideos -> do
      home <- liftIO getHomeDirectory
      pure $ home ++ "/Videos"
  pure $ intercalate "/" $ rootAbsPath : (T.unpack . unwrap <$> dirNames)

data VideoFilePath = VideoFilePath
  { videoFilePathRoot :: RootDirectoryLocation,
    videoFilePathNames :: [DirectoryName],
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq, Generic)

instance ToJSON VideoFilePath where
  toEncoding (VideoFilePath root dirNames videoName) =
    toEncoding . unsplitSeparatedText $
      unRootDirectoryLocation root
        : (unDirectoryName <$> dirNames)
        ++ [videoName.unVideoFileName]

instance FromJSON VideoFilePath where
  parseJSON = withText "VideoFilePath" $ \text -> do
    let pieces = splitAtSeparatorNE text
        fileNameT = NE.last pieces
        fileName = VideoFileName fileNameT
        isVideoFile :: Bool
        isVideoFile = any ((`T.isSuffixOf` unwrap fileNameT) . T.pack) videoFileExts
        dirPathPieces = NE.nonEmpty $ NE.init pieces
    if isVideoFile
      then case parseRootAndNames <$> dirPathPieces of
        Just (Right (root, names)) -> pure $ VideoFilePath root names fileName
        Just (Left err) -> parseFail $ "Couldn't parse VideoFilePath: " ++ err
        Nothing -> parseFail "Couldn't parse VideoFilePath, no directory found in path."
      else parseFail "Not a video file, wrong extension."

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
  pure $ dirAbsPath ++ "/" ++ T.unpack (unwrap fileName)

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

-- | Looks for an image in the directory or any of it's parents, preferring closer to the directory
findDirWithImageFor :: RootDirectories -> DirectoryPath -> Maybe DirectoryPath
findDirWithImageFor allRoots (DirectoryPath dirRoot dirNames) = innerSearch (NE.nonEmpty dirNames)
  where
    innerSearch Nothing = Nothing
    innerSearch (Just dirNamesNE) = do
      let dirPath = DirectoryPath dirRoot $ NE.toList dirNamesNE
      case getDirectoryAtPath allRoots dirPath of
        Nothing -> Nothing
        Just d ->
          case d.directoryImage of
            Nothing -> innerSearch (NE.nonEmpty $ NE.init dirNamesNE)
            Just _img -> Just dirPath

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
  | LikelyOther T.Text

-- | This expects a string that's the result of a `listDirectory` call
guessType :: String -> LikelyPathType
guessType str = case str of
  "" -> LikelyOther txt
  '.' : _ -> LikelyOther txt
  _ -> case takeExtension str of
    ext | isVideoFileExt ext -> mkGuess $ LikelyVideoFile . VideoFileName
    ext | isImageFileExt ext -> mkGuess $ LikelyImageFile . ImageFileName
    ext | isCommonFileExt ext -> LikelyOther txt
    _ -> mkGuess $ LikelyDir . DirectoryName
  where
    txt = T.pack str
    -- Turns the string into a guess, either a guess of the given constructor,
    -- or LikelyOther if there were separators in the string.
    mkGuess :: (TextWithoutSeparator -> LikelyPathType) -> LikelyPathType
    mkGuess constr =
      case textWithoutSeparator txt of
        Nothing -> LikelyOther txt
        Just tws -> constr tws

partitionPathTypes :: [LikelyPathType] -> ([DirectoryName], [VideoFileName], [ImageFileName], [T.Text])
partitionPathTypes =
  foldr
    ( \guess (dirs, vids, imgs, oths) -> case guess of
        LikelyDir d -> (d : dirs, vids, imgs, oths)
        LikelyVideoFile v -> (dirs, v : vids, imgs, oths)
        LikelyImageFile i -> (dirs, vids, i : imgs, oths)
        LikelyOther t -> (dirs, vids, imgs, t : oths)
    )
    ([], [], [], [])

bestImageFile :: [ImageFileName] -> Maybe ImageFileName
bestImageFile = safeMinimumOn $ \fileName ->
  case lower $ takeBaseName $ T.unpack $ unwrap fileName of
    "poster" -> 0 :: Int
    "cover" -> 1
    _ -> 100

data DirectoryKindGuess
  = DirectoryKindMovie Text -- Best guess for the movie title
  | DirectoryKindSeries Text -- Best guess for the series' name
  | DirectoryKindSeriesSeason -- For now we don't need any extra info about seasons
  | DirectoryKindUnknown

guessDirectoryKind :: DirectoryName -> DirectoryData -> DirectoryKindGuess
guessDirectoryKind dirName dirData =
  let (title, _rest) = splitTitleFromDir dirName
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
    result <- updateDirectoryFromDisk rootDirs path rootAsDir
    case result of
      DirectoryChanged updated ->
        pure
          RootDirectoryData
            { rootDirectorySubDirs = updated.directorySubDirs,
              rootDirectoryVideoFiles = updated.directoryVideoFiles
            }
      DirectoryUnchanged -> pure rootDirData
      DirectoryNotADirectory -> do
        putLog Warning $
          unwords
            [ "Root directory",
              T.unpack $ unwrap $ unRootDirectoryLocation rootLocation,
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
updateDirectoryFromDisk :: forall m. (MonadIO m, MonadThrow m, Logger m) => RootDirectories -> DirectoryPath -> DirectoryData -> m DirectoryUpdateResult
updateDirectoryFromDisk allDirsData dirPath dir = do
  absDirPath <- directoryPathToAbsPath dirPath
  logDuration ("Updated directory " ++ absDirPath) $ do
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
        subDirUpdates <- forM subDirGuesses $ \subDirName -> do
          let knownDir = Map.lookup subDirName dir.directorySubDirs
          let newDir = DirectoryData Nothing Map.empty Map.empty
          let fullPath = addSubDir dirPath subDirName
          upd <- updateDirectoryFromDisk allDirsData fullPath $ knownDir `orElse` newDir
          pure (subDirName, upd)
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
              let fullFilePath = absDirPath ++ "/" ++ T.unpack (unwrap newVideoName.unVideoFileName)
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
        let diskImageName = bestImageFile imageGuesses
            currentImageName = fst <$> dir.directoryImage
            anyParentDirHasImage = isJust $ findDirWithImageFor allDirsData dirPath
            isRootDir = null dirPath.directoryPathNames
            isSeasonDirOrSubDir = any isSeasonDir dirPath.directoryPathNames

            getImageFromDisk :: ImageFileName -> m (ImageFileName, ImageFileData)
            getImageFromDisk imgName = do
              putLog Info $ "Getting image from disk for: " ++ absDirPath
              imgFile <- liftIO $ BS.readFile $ absDirPath </> T.unpack (unwrap imgName)
              pure (imgName, ImageFileData imgFile)

            imgFindingErr msg = "Failed finding image for " ++ absDirPath ++ ": " ++ msg
            tryFindImage' searchTerm = do
              mImg <- tryFindImage searchTerm
              case mImg of
                Left ImageSearchFailedScraping -> do
                  putLog Warning $ imgFindingErr "web scraping unsuccessful"
                  pure Nothing
                Left (ImageSearchDownloadFailed err) -> do
                  putLog Warning $ imgFindingErr err
                  pure Nothing
                Right (contentType, img) -> do
                  putLog Info $ "Downloaded image for " ++ absDirPath ++ " with search term " ++ T.unpack searchTerm
                  pure $ Just (imageFileNameForContentType contentType, ImageFileData img)

            dirName =
              NE.last $
                unRootDirectoryLocation dirPath.directoryPathRoot
                  NE.:| map unDirectoryName dirPath.directoryPathNames
            getImageFromWeb = do
              firstAttempt <- tryFindImage' $ unTextWithoutSeparator dirName
              case firstAttempt of
                Just img -> pure $ Just img
                Nothing -> tryFindImage' . fst $ splitTitleFromDir $ DirectoryName dirName
        newImage <- case (currentImageName, diskImageName) of
          (Nothing, Just diskImg) ->
            -- We currently don't know about an image, but there's one on disk, use that.
            Just <$> getImageFromDisk diskImg
          (Just img, Just diskImg)
            | img /= diskImg ->
                -- The image we know about is different from the one on disk, update it!
                Just <$> getImageFromDisk diskImg
          (Just _img, Just _diskImg) ->
            -- The image we know about is the same as on disk, so no need to update, save some IO
            pure Nothing
          (Just _img, Nothing) ->
            -- We have an image already, but none on disk. Just keep it, we probably downloaded it previously.
            pure Nothing
          (Nothing, Nothing)
            | isRootDir -- Root dirs never have images
                || isSeasonDirOrSubDir -- Season dirs or subdirs of season don't have images, otherwise we search for "Season X" a bunch. Subdirs are likely stuff like "subs"
                || anyParentDirHasImage -> -- If the parent dir already has an image we use that image, so save some time and don't search for another image
                pure Nothing
          (Nothing, Nothing) ->
            -- Nothing known, nothing on disk, try and download one
            getImageFromWeb

        case (updatedSubDirs, updatedVideoFiles, newImage) of
          (Nothing, Nothing, Nothing) -> pure DirectoryUnchanged
          _ -> do
            pure . DirectoryChanged $
              DirectoryData
                { directoryImage = newImage,
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

isSeasonDir :: DirectoryName -> Bool
isSeasonDir = isJust . seasonFromDir

seasonFromDir :: DirectoryName -> Maybe Int
seasonFromDir dir =
  tryRegex (unwrap dir) expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex (unwrap dir) expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex (unwrap dir) expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: [VideoFileName] -> Maybe Int
seasonFromFiles fileNames =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ NE.group (mapMaybe seasonFromFile fileNames))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) fileNames

    seasonFromFile :: VideoFileName -> Maybe Int
    seasonFromFile file =
      tryRegex (unwrap file) expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex (unwrap file) expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: VideoFileName -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile file =
  let double :: Maybe (Int, Int, Int)
      double =
        tryRegex (unwrap file) expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      seasonAndEp :: Maybe (Int, Int)
      seasonAndEp =
        tryRegex (unwrap file) expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex (unwrap file) expect2Ints "([0-9]+)[Xx]([0-9]+)"

      epOnly :: Maybe Int
      epOnly =
        tryRegex (unwrap file) expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex (unwrap file) expect1Int "[Aa]flevering ([0-9]+)"
   in case double of
        Just (s, a, b) -> (Just s, Just $ Right (a, b))
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ Left a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ Left a)
            Nothing -> (Nothing, Nothing)

splitTitleFromDir :: DirectoryName -> (Text, Text)
splitTitleFromDir dirName =
  let (title, rest) = T.breakOn "(" $ unwrap dirName
   in (T.strip title, T.strip rest)

niceFileNameT :: VideoFileName -> Text
niceFileNameT file =
  T.replace "." " " $ T.pack $ takeBaseName $ T.unpack $ unwrap file

isVideoFileExt :: String -> Bool
isVideoFileExt ext = ext `Set.member` videoFileExts

videoFileExts :: Set.HashSet String
videoFileExts =
  Set.fromList
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
isImageFileExt ext = ext `Set.member` imageFileExts

imageFileExts :: Set.HashSet String
imageFileExts =
  Set.fromList
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
isCommonFileExt ext = ext `Set.member` commonFileExts

commonFileExts :: Set.HashSet String
commonFileExts =
  Set.fromList
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
