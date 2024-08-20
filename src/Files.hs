module Files
  ( DirectoryInfo (..),
    DirectoryKind (..),
    parseDirectory,
    guessDirectoryInfo,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), genericParseJSON, genericToJSON)
import Data.List (sort)
import Data.List.NonEmpty (group, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text, breakOn, replace, strip)
import Data.Text qualified as T
import Data.Yaml (decodeFileEither, encodeFile)
import GHC.Data.Maybe (firstJusts, firstJustsM, listToMaybe, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Utils.Monad (partitionM)
import JSON (prefixedDefaultOptions)
import Path
import System.Directory (doesFileExist, listDirectory, renameFile)
import System.FilePath (combine, dropTrailingPathSeparator)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data DirectoryInfo = DirectoryInfo
  { directoryInfoKind :: DirectoryKind,
    directoryInfoTitle :: Text,
    directoryInfoImage :: Maybe Text,
    directoryInfoDifferentiator :: Maybe Text,
    directoryInfoDescription :: Maybe Text,
    directoryInfoImdb :: Maybe Text,
    directoryInfoTvdb :: Maybe Text,
    directoryInfoTmdb :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON DirectoryInfo where
  parseJSON = genericParseJSON $ prefixedDefaultOptions 13

instance ToJSON DirectoryInfo where
  toJSON = genericToJSON $ prefixedDefaultOptions 13

data DirectoryKind
  = DirectoryKindMovie
  | DirectoryKindSeries
  deriving (Generic, Show, Eq)

instance FromJSON DirectoryKind where
  parseJSON = genericParseJSON $ prefixedDefaultOptions 13

instance ToJSON DirectoryKind where
  toJSON = genericToJSON $ prefixedDefaultOptions 13

-- | The main function, gives a directory, tries to see if there's existing info,
-- and if not tries to guess it and saves it.
parseDirectory :: Path Abs Dir -> IO (Maybe DirectoryInfo, [(Text, Path Rel File)], [(Text, Path Rel Dir)])
parseDirectory dir = do
  -- Read files and directories
  fileAndDirectoryNames <- listDirectory $ fromAbsDir dir
  (fileNames, directoryNames) <- partitionM (doesFileExist . combine (fromAbsDir dir)) fileAndDirectoryNames
  files <- sort . filter isVideoFile <$> mapM parseRelFile fileNames
  directories <- sort <$> mapM parseRelDir directoryNames

  -- TODO: Make proper way of cleaning names, such as including season info if we have it
  -- But for now we just use the filename
  let filesWithNames = map (\f -> (niceFileNameT f, f)) files
      directoriesWithNames = map (\d -> (niceDirNameT d, d)) directories
      -- Guess some info in case there's no info file
      mInfoGuess = guessDirectoryInfo dir files directories

      handleNoExistingInfo rootDir info = do
        -- Write the info, that way we have a template to fill in the data
        -- TODO: Do a call to the TVDB or something to get better data
        encodeFile (combine (fromAbsDir rootDir) "info.yaml") info
        pure (Just info, filesWithNames, directoriesWithNames)

  -- See if there's an info file
  mInfoFile <- tryGetFile (== "info.yaml") dir

  case mInfoFile of
    Just infoFile -> do
      decodedOrError <- decodeFileEither (fromAbsFile infoFile)
      case decodedOrError of
        Right info ->
          -- We have a file and it coded correctly, so we use that
          pure (Just info, filesWithNames, directoriesWithNames)
        -- We have a file, but it failed to decode, so we use the guess
        Left _ -> case mInfoGuess of
          Nothing ->
            -- Couldn't guess either, so I guess we have nothing
            pure (Nothing, filesWithNames, directoriesWithNames)
          Just (rootDir, info) -> do
            -- The file failed to decode, but we do have a guess, so make a backup
            -- and then save the guess
            newName <- addExtension ".backup" infoFile
            renameFile (fromAbsFile infoFile) (fromAbsFile newName)
            handleNoExistingInfo rootDir info
    Nothing ->
      case mInfoGuess of
        Nothing -> pure (Nothing, filesWithNames, directoriesWithNames)
        Just (rootDir, info) -> handleNoExistingInfo rootDir info

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

movieYearFromDir :: Path a Dir -> Maybe Int
movieYearFromDir dir =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex (dirname dir) expect2Ints "((19|20)[0-9][0-9])"

movieYearFromFiles :: [Path a File] -> Maybe Int
movieYearFromFiles files =
  firstJusts (try <$> files)
  where
    try file = fst <$> tryRegex file expect2Ints "((19|20)[0-9][0-9])"

movieTitleFromDir :: Path a Dir -> Text
movieTitleFromDir folder = strip . fst $ breakOn "(" (niceDirNameT folder)

tryGetFile :: (FilePath -> Bool) -> Path Abs Dir -> IO (Maybe (Path Abs File))
tryGetFile predicate startDir =
  -- Look up to X levels deep
  firstJustsM $ tryGetFile' <$> take 3 (iterate parent startDir)
  where
    tryGetFile' dir = do
      fileAndDirNames <- listDirectory $ fromAbsDir dir
      let matchingFileNames = filter predicate fileAndDirNames
      foundFile <- listToMaybe <$> mapM parseRelFile matchingFileNames
      pure $ (dir </>) <$> foundFile

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

guessDirectoryInfo :: Path a Dir -> [Path Rel File] -> [Path Rel Dir] -> Maybe (Path a Dir, DirectoryInfo)
guessDirectoryInfo dir files directories =
  let videoFiles :: [Path Rel File]
      videoFiles = sort $ filter isVideoFile files

      isSeriesDir = any (isJust . seasonFromDir) directories

      mSeasonFromDir = seasonFromDir dir
      mSeasonFromFiles = seasonFromFiles videoFiles
      mSeason = mSeasonFromDir <|> mSeasonFromFiles

      simpleInfo kind name =
        DirectoryInfo
          { directoryInfoKind = kind,
            directoryInfoTitle = name,
            directoryInfoImage = Nothing,
            directoryInfoDifferentiator = Nothing,
            directoryInfoDescription = Nothing,
            directoryInfoImdb = Nothing,
            directoryInfoTvdb = Nothing,
            directoryInfoTmdb = Nothing
          }
   in case (mSeason, nonEmpty videoFiles, nonEmpty directories) of
        (Just _season, Just _actualFiles, _) ->
          Just
            ( if isJust mSeasonFromDir
                then parent dir
                else dir,
              simpleInfo DirectoryKindSeries $
                if isJust mSeasonFromDir
                  then niceDirNameT $ parent dir
                  else niceDirNameT dir
            )
        (_, _, _)
          | isSeriesDir ->
              Just
                ( dir,
                  simpleInfo DirectoryKindSeries $ niceDirNameT dir
                )
        (_, Just _actualFiles, _) ->
          Just
            ( dir,
              let base = simpleInfo DirectoryKindMovie $ movieTitleFromDir dir
               in base
                    { directoryInfoDifferentiator = fmap (T.pack . show) $ movieYearFromDir dir <|> movieYearFromFiles videoFiles
                    }
            )
        _ -> Nothing
