{-# LANGUAGE TemplateHaskell #-}

module Directory
  ( DirectoryRaw (..),
    readDirectoryRaw,
    DirectoryInfo (..),
    DirectoryKind (..),
    readDirectoryInfoRec,
    updateAllDirectoryInfos,
    getVideoDirPath,
    niceFileNameT,
    niceDirNameT,
    -- For testing
    guessDirectoryInfo,
    isVideoFile,
  )
where

import Algorithms.NaturalSort qualified as Natural
import Autodocodec
  ( HasCodec (codec),
    object,
    optionalFieldOrNull,
    requiredField,
    stringConstCodec,
    (.=),
  )
import Autodocodec.Codec (optionalFieldWithDefaultWith)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec, encodeYamlViaCodec)
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.Extra (whenJust)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortBy)
import Data.List.NonEmpty (group)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, breakOn, replace, strip)
import Data.Text qualified as T
import GHC.Data.Maybe
  ( firstJusts,
    fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    orElse,
  )
import GHC.Exts (sortWith)
import Path (Abs, Dir, File, Path, Rel, addExtension, dirname, fileExtension, filename, fromRelDir, fromRelFile, mkRelDir, mkRelFile, parent, parseRelFile, splitExtension, toFilePath, (</>))
import SaferIO (FSRead (..), FSWrite (..), Logger (..), NetworkRead)
import System.FilePath (dropTrailingPathSeparator)
import TVDB (TVDBData (..), TVDBToken, TVDBType (..), getInfoFromTVDB)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Yesod (ContentType)

data DirectoryInfo = DirectoryInfo
  { directoryInfoKind :: DirectoryKind,
    directoryInfoTitle :: Text,
    directoryInfoYear :: Maybe Int,
    directoryInfoDescription :: Maybe Text,
    directoryInfoImdb :: Maybe Text,
    directoryInfoTvdb :: Maybe Text,
    directoryInfoTmdb :: Maybe Text,
    directoryInfoForceUpdate :: Maybe Bool
  }
  deriving (Show, Eq)

instance HasCodec DirectoryInfo where
  codec =
    object "DirectoryInfo" $
      DirectoryInfo
        <$> requiredField "kind" "Is this a series or a movie?" .= directoryInfoKind
        <*> requiredField "title" "The title of this series or movie" .= directoryInfoTitle
        <*> optionalFieldWriteNull "year" "The year this series or movie was released" .= directoryInfoYear
        <*> optionalFieldWriteNull "description" "The description of the series or movie" .= directoryInfoDescription
        <*> optionalFieldOrNull "imdb" "The IMDB ID" .= directoryInfoImdb
        <*> optionalFieldOrNull "tvdb" "The TVDB ID" .= directoryInfoTvdb
        <*> optionalFieldOrNull "tmdb" "The TMDB ID" .= directoryInfoTmdb
        <*> optionalFieldOrNull "force-update" "This forces the system to try and download more information again. This can be used for when some of the data was wrong, you can remove the incorrect data, fill in what you know, set this flag to true, and it'll use the info you gave it to search for the rest." .= directoryInfoForceUpdate
    where
      -- We use this instead of `optionalFieldOrNull` because this also writes a `null` when the field is `Nothing`
      -- that way we get `null` values in new files and they serve as better templates, but we can still
      -- remove the field from the file and they'll parse just fine.
      optionalFieldWriteNull key = optionalFieldWithDefaultWith key codec Nothing

data DirectoryKind
  = DirectoryKindMovie
  | DirectoryKindSeries
  deriving (Show, Eq)

instance HasCodec DirectoryKind where
  codec =
    stringConstCodec $
      (DirectoryKindMovie, "movie")
        NE.:| [ (DirectoryKindSeries, "series")
              ]

data DirectoryRaw = DirectoryRaw
  { directoryPath :: Path Abs Dir,
    directoryVideoFiles :: [Path Rel File],
    directoryDirectories :: [Path Rel Dir]
  }
  deriving (Show, Eq)

getVideoDirPath :: (FSRead m) => m (Path Abs Dir)
getVideoDirPath = do
  home <- getHomeDir
  let videoDirName = $(mkRelDir "Videos")
  pure $ home </> videoDirName

-- | This reads a directory and returns the video files and directories in it.
-- Also sorts these lists in a natural way (putting 2 before 10, for example).
readDirectoryRaw :: (FSRead m) => Path Abs Dir -> m DirectoryRaw
readDirectoryRaw dir = do
  (dirNames, fileNames) <- listDirRel dir
  let files = smartFileSort $ filter isVideoFile fileNames
  let directories = smartDirSort dirNames
  pure $ DirectoryRaw dir files directories

-- | This reads the info file from a directory and returns it if it exists.
-- If a file with the correct name exists, but it can't be decoded, it will
-- also return `Nothing` and print an error in the console.
readDirectoryInfo :: (FSRead m, Logger m) => Path Abs Dir -> m (Maybe DirectoryInfo)
readDirectoryInfo dir = do
  mInfoFile <- readFileBSSafe $ mkDirInfoFilePath dir
  case mInfoFile of
    Nothing -> pure Nothing
    Just infoFile ->
      case eitherDecodeYamlViaCodec infoFile of
        Right info -> pure $ Just info
        Left err -> do
          logStr $ "Failed to decode info file: " ++ show err
          pure Nothing

-- | Try and read directory info, if not found recursively try the parent folder
-- until some info is found, or return nothing if no info is ever found.
-- Returns the directory the file was found in.
readDirectoryInfoRec :: (FSRead m, Logger m) => Path Abs Dir -> m (Maybe (Path Abs Dir, DirectoryInfo))
readDirectoryInfoRec dir = do
  mInfoFile <- readDirectoryInfo dir
  case mInfoFile of
    Just info -> pure $ Just (dir, info)
    Nothing ->
      let parentDir = parent dir
       in -- The `parent` function returns the same dir if it's the root of your file system
          -- so at that point we don't want to loop forever
          if parentDir == dir
            then pure Nothing
            else readDirectoryInfoRec parentDir

type Image = (ContentType, BS.ByteString)

-- | This downloads more information about a directory from TVDB and returns it.
-- This will always try and download info, even if it's already available, to
-- see if there's any update to the values.
downloadDirectoryInfo :: (NetworkRead m, Logger m) => TVDBToken -> DirectoryInfo -> m (DirectoryInfo, Maybe Image)
downloadDirectoryInfo tvdbToken startingInfo = do
  let tvdbType = case directoryInfoKind startingInfo of
        DirectoryKindMovie -> TVDBTypeMovie
        DirectoryKindSeries -> TVDBTypeSeries
  (usedInfo, mTVDBData) <- do
    let getInfoForYear = getInfoFromTVDB tvdbToken (directoryInfoTitle startingInfo) tvdbType
    attemptWithYear <- getInfoForYear startingInfo.directoryInfoYear
    case attemptWithYear of
      Just d -> pure (startingInfo, Just d)
      -- Fallback to not using the year in case we guessed it wrong.
      -- I had this happen when a series had a special episode that had a year in the name, but that was the year
      -- the special was made, not the series.
      Nothing | isJust startingInfo.directoryInfoYear -> do
        attemptWithoutYear <- getInfoForYear Nothing
        pure (startingInfo {directoryInfoYear = Nothing}, attemptWithoutYear)
      Nothing -> pure (startingInfo, Nothing)

  -- If we're doing a force update, that means someone corrected it manually, so we do not want to overwrite
  let select infoField tvdbValue =
        if isNothing $ infoField usedInfo
          then tvdbValue
          else infoField usedInfo

  let extendedInfo = case mTVDBData of
        Nothing ->
          usedInfo
            { -- Even when we don't find anything, remove this flag so we don't keep trying
              directoryInfoForceUpdate = Nothing
            }
        Just tvdbData ->
          DirectoryInfo
            { directoryInfoKind = usedInfo.directoryInfoKind,
              directoryInfoTitle = usedInfo.directoryInfoTitle,
              directoryInfoYear = select directoryInfoYear (tvdbData.tvdbDataYear <|> usedInfo.directoryInfoYear),
              directoryInfoDescription = select directoryInfoDescription tvdbData.tvdbDataDescription,
              directoryInfoImdb = select directoryInfoImdb tvdbData.tvdbDataImdb,
              directoryInfoTvdb = select directoryInfoTvdb (Just tvdbData.tvdbDataId),
              directoryInfoTmdb = select directoryInfoTmdb tvdbData.tvdbDataTmdb,
              directoryInfoForceUpdate = Nothing
            }

  pure (extendedInfo, mTVDBData >>= tvdbDataImage)

writeDirectoryInfo :: (FSWrite m) => Path Abs Dir -> DirectoryInfo -> Maybe Image -> m ()
writeDirectoryInfo dir info mImage = do
  writeFileBS (mkDirInfoFilePath dir) (encodeYamlViaCodec info)
  case mImage of
    Just (contentType, imgBytes) -> writeImage dir contentType imgBytes
    Nothing -> pure ()

-- | Reads the existing directory info, if it exists and update is wanted it does,
-- if not makes a guess and tries to get data online.
-- Returns the updated (or existing) info.
updateDirectoryInfo :: (FSRead m, NetworkRead m, FSWrite m, Logger m) => TVDBToken -> Path Abs Dir -> m DirectoryInfo
updateDirectoryInfo tvdbToken dir = do
  existingInfo <- readDirectoryInfo dir
  case existingInfo of
    Just info
      | info.directoryInfoForceUpdate == Just True -> do
          updateAndWrite info
    Just info ->
      pure info
    Nothing -> do
      dirRaw <- readDirectoryRaw dir
      let guessedInfo = guessDirectoryInfo dirRaw
      updateAndWrite guessedInfo
  where
    updateAndWrite info = do
      -- Make a backup if a file already exists,
      -- do this without parsing it, as it might (have) fail(ed) parsing
      let infoFilePath = mkDirInfoFilePath dir
      case addExtension ".backup" infoFilePath of
        Just backupPath ->
          renameFileSafe infoFilePath backupPath
        Nothing -> logStr $ "Failed to make backup file name: " ++ toFilePath infoFilePath
      -- Get the new data and write it to disk
      (extendedInfo, mImage) <- downloadDirectoryInfo tvdbToken info
      writeDirectoryInfo dir extendedInfo mImage
      pure extendedInfo

-- | This updates all directory infos in the video directory.
-- It will optionally download more information from TVDB, and write the new info to disk.
-- The refresh level can be overwritten by setting `force-update` in the info file.
updateAllDirectoryInfos :: (FSRead m, NetworkRead m, FSWrite m, Logger m) => TVDBToken -> m [(Path Abs Dir, DirectoryInfo)]
updateAllDirectoryInfos tvdbToken = do
  videoDirPath <- getVideoDirPath
  videoDirRaw <- readDirectoryRaw videoDirPath

  whenJust (NE.nonEmpty videoDirRaw.directoryVideoFiles) $ \files ->
    logStr . unlines $
      [ "Found video files in root video folder.",
        "You'll still see these as videos on the main page, but no metadata will be downloaded for them.",
        "Place them in a folder with the movie or series name to get metadata.",
        "The files are:"
      ]
        ++ NE.toList (fromRelFile <$> files)

  forM videoDirRaw.directoryDirectories $ \dir -> do
    let absPath = videoDirPath </> dir
    info <- updateDirectoryInfo tvdbToken absPath
    pure (absPath, info)

dirInfoFileName :: Path Rel File
dirInfoFileName = $(mkRelFile "info.yaml")

mkDirInfoFilePath :: Path Abs Dir -> Path Abs File
mkDirInfoFilePath root = root </> dirInfoFileName

writeImage :: (FSWrite m) => Path Abs Dir -> ContentType -> BS.ByteString -> m ()
writeImage rootDir contentType imgBytes =
  let extension' =
        if BS.isPrefixOf "image/" contentType
          then Just $ BS.drop 6 contentType
          else Nothing
      extension = if extension' == Just "jpeg" then Just "jpg" else extension'
      fallbackName = $(mkRelFile "poster.jpg")
      name = fromMaybe fallbackName $ do
        ext <- extension
        parseRelFile $ BS8.unpack $ "poster." <> ext

      path = rootDir </> name
   in writeFileBS path imgBytes

guessDirectoryKind :: DirectoryRaw -> DirectoryKind
guessDirectoryKind dirRaw =
  let isSeriesDir = any (isJust . seasonFromDir) dirRaw.directoryDirectories
      mSeasonFromFiles = seasonFromFiles dirRaw.directoryVideoFiles
   in if isSeriesDir || isJust mSeasonFromFiles
        then DirectoryKindSeries
        else DirectoryKindMovie

-- | This tries to guess some information based on the directory and file names.
guessDirectoryInfo :: DirectoryRaw -> DirectoryInfo
guessDirectoryInfo dirRaw =
  let dir = dirRaw.directoryPath
      videoFiles = dirRaw.directoryVideoFiles
      kind = guessDirectoryKind dirRaw
   in DirectoryInfo
        { directoryInfoKind = kind,
          directoryInfoTitle = titleFromDir dir,
          directoryInfoYear = yearFromDir dir <|> yearFromFiles videoFiles,
          directoryInfoDescription = Nothing,
          directoryInfoImdb = Nothing,
          directoryInfoTvdb = Nothing,
          directoryInfoTmdb = Nothing,
          directoryInfoForceUpdate = Nothing
        }

-- Some helpers

readInt :: String -> Maybe Int
readInt = readMaybe

tryRegex :: Path x y -> ([String] -> Maybe a) -> String -> Maybe a
tryRegex source resultParser regex =
  let res :: (String, String, String, [String])
      res = toFilePath source =~ regex
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

seasonFromDir :: Path a Dir -> Maybe Int
seasonFromDir dir =
  tryRegex dir expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: [Path a File] -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

    seasonFromFile :: Path a File -> Maybe Int
    seasonFromFile file =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: Path a File -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile file =
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

yearRegex :: String
yearRegex = "((19|20)[0-9][0-9])"

yearFromDir :: Path a Dir -> Maybe Int
yearFromDir dir =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex (dirname dir) expect2Ints yearRegex

yearFromFiles :: [Path a File] -> Maybe Int
yearFromFiles files =
  firstJusts (try <$> files)
  where
    try file = fst <$> tryRegex file expect2Ints yearRegex

titleFromDir :: Path a Dir -> Text
titleFromDir folder = strip . fst $ breakOn "(" (niceDirNameT folder)

niceDirNameT :: Path a Dir -> Text
niceDirNameT = T.pack . dropTrailingPathSeparator . fromRelDir . dirname

niceFileNameT :: Path a File -> Text
niceFileNameT file =
  let withoutExt = (fst <$> splitExtension file) `orElse` file
      name = fromRelFile $ filename withoutExt
   in replace "." " " $ T.pack name

isVideoFile :: Path b File -> Bool
isVideoFile file =
  case fileExtension file of
    Just ext -> ext `elem` [".mp4", ".mkv", ".avi", ".webm"]
    Nothing -> False

-- | Sorts the dirs, taking into account numbers properly
smartDirSort :: [Path Rel Dir] -> [Path Rel Dir]
smartDirSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceDirNameT a)
        (T.toLower $ niceDirNameT b)

-- | Sorts the dirs, taking into account numbers properly
smartFileSort :: [Path Rel File] -> [Path Rel File]
smartFileSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceFileNameT a)
        (T.toLower $ niceFileNameT b)
