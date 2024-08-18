module Files
  ( DirectoryInfo (..),
    EpisodeInfo (..),
    EpisodeNumber (..),
    MovieInfo (..),
    parseDirectory,
    parseDirectory',
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Options (..), ToJSON (toEncoding), defaultOptions, genericParseJSON, genericToEncoding)
import Data.Char (toLower)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty, group, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text, breakOn, strip)
import Data.Text qualified as T
import GHC.Data.Maybe (listToMaybe, orElse)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Utils.Monad (partitionM)
import Path
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (combine, dropTrailingPathSeparator)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data EpisodeInfo = EpisodeInfo
  { -- It's possible a folder contains episodes from multiple seasons,
    -- so we store the season number per episode.
    -- When a folder only contains specials, and no other indication of season,
    -- the season is set to 0. If there is an indication, we set the correct
    -- season number. In either case the `special` flag will be set too.
    episodeSeason :: Int,
    episodeNumber :: EpisodeNumber,
    episodeSpecial :: Bool,
    episodeFileName :: Path Rel File
  }
  deriving (Eq, Show)

instance Ord EpisodeInfo where
  compare a b =
    -- Season 0 is an indicator for specials when we don't know the season
    ( case (a.episodeSeason, b.episodeSeason) of
        (0, 0) -> EQ
        (0, _) -> GT
        (_, 0) -> LT
        (x, y) -> compare x y
    )
      <> compare a.episodeSpecial b.episodeSpecial
      <> compare a.episodeNumber b.episodeNumber
      <> compare a.episodeFileName b.episodeFileName

data EpisodeNumber
  = EpisodeNumber Int
  | EpisodeNumberDouble Int Int
  deriving (Eq, Show)

instance Ord EpisodeNumber where
  compare :: EpisodeNumber -> EpisodeNumber -> Ordering
  compare (EpisodeNumber a) (EpisodeNumberDouble c d) = compare a c <> compare a d
  compare (EpisodeNumberDouble c d) (EpisodeNumber a) = compare c a <> compare d a
  compare (EpisodeNumber a) (EpisodeNumber b) = compare a b
  compare (EpisodeNumberDouble a b) (EpisodeNumberDouble c d) = compare a c <> compare b d

newtype MovieInfo = MovieInfo
  { movieFileName :: Path Rel File
  }
  deriving (Eq, Show, Ord)

data DirectoryInfoFileKind
  = DirectoryInfoFileKindMovie
  | DirectoryInfoFileKindSeries
  deriving (Generic)

instance FromJSON DirectoryInfoFileKind where
  parseJSON = genericParseJSON $ prefixedDefaultOptions 21

instance ToJSON DirectoryInfoFileKind where
  toEncoding = genericToEncoding $ prefixedDefaultOptions 21

data DirectoryInfoFile = DirectoryInfoFile
  { directoryInfoFileKind :: DirectoryInfoFileKind,
    directoryInfoFileTitle :: Text,
    directoryInfoFileDifferentiator :: Maybe Text,
    directoryInfoFileDescription :: Maybe Text,
    directoryInfoFileImdb :: Maybe Text,
    directoryInfoFileTvdb :: Maybe Text,
    directoryInfoFileTmdb :: Maybe Text
  }
  deriving (Generic)

prefixedDefaultOptions :: Int -> Options
prefixedDefaultOptions n =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop n,
      constructorTagModifier = lowerFirst . drop n,
      omitNothingFields = True
    }
  where
    lowerFirst "" = ""
    lowerFirst (x : xs) = toLower x : xs

instance FromJSON DirectoryInfoFile where
  parseJSON = genericParseJSON $ prefixedDefaultOptions 17

instance ToJSON DirectoryInfoFile where
  toEncoding = genericToEncoding $ prefixedDefaultOptions 17

data DirectoryInfo
  = SeriesDirectory
      { seriesTitle :: Text
      }
  | SeasonDirectory
      { seasonSeriesTitle :: Text,
        seasonEpisodes :: NonEmpty EpisodeInfo
      }
  | MovieDirectory
      { movieTitle :: Text,
        movieYear :: Maybe Int,
        movieFiles :: NonEmpty MovieInfo
      }
  deriving (Eq, Show, Ord)

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

-- Getting the info

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
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: Path a File -> (Maybe Int, Maybe EpisodeNumber)
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
        Just (s, a, b) -> (Just s, Just $ EpisodeNumberDouble a b)
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ EpisodeNumber a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ EpisodeNumber a)
            Nothing -> (Nothing, Nothing)

isSpecialFromFile :: Path a File -> Bool
isSpecialFromFile file =
  toFilePath file =~ ("[Ss]pecial" :: Text)
    || toFilePath file =~ ("[Ss]0+[Ee][0-9]+" :: Text)
    || toFilePath file =~ ("0+[Xx][0-9]+" :: Text)

movieYearFromDir :: Path a Dir -> Maybe Int
movieYearFromDir dir =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex (dirname dir) expect2Ints "((19|20)[0-9][0-9])"

movieTitleFromDir :: Path a Dir -> Text
movieTitleFromDir folder = strip . fst $ breakOn "(" (niceDirNameT folder)

parseDirectory :: Path Abs Dir -> IO (Maybe DirectoryInfo, [Path Rel Dir])
parseDirectory dir = do
  fileAndDirectoryNames <- listDirectory $ fromAbsDir dir
  (fileNames, directoryNames) <- partitionM (doesFileExist . combine (fromAbsDir dir)) fileAndDirectoryNames

  files <- mapM parseRelFile fileNames
  directories <- mapM parseRelDir directoryNames

  pure (parseDirectory' dir files directories, sort directories)

niceDirNameT :: Path a Dir -> Text
niceDirNameT = T.pack . dropTrailingPathSeparator . fromRelDir . dirname

parseDirectory' :: Path a Dir -> [Path Rel File] -> [Path Rel Dir] -> Maybe DirectoryInfo
parseDirectory' dir files directories =
  let videoFiles :: [Path Rel File]
      videoFiles = sort $ filter isVideoFile files
      isVideoFile file = elem (fileExtension file `orElse` "") [".mp4", ".mkv", ".avi", ".webm"]

      isSeriesDir = any (isJust . seasonFromDir) directories

      mSeasonFromDir = seasonFromDir dir
      mSeasonFromFiles = seasonFromFiles videoFiles
      mSeason = mSeasonFromDir <|> mSeasonFromFiles
   in case (mSeason, nonEmpty videoFiles, nonEmpty directories) of
        (Just season, Just actualFiles, _) ->
          Just
            SeasonDirectory
              { seasonSeriesTitle =
                  if isJust mSeasonFromDir
                    then niceDirNameT $ dirname $ parent dir
                    else niceDirNameT $ dirname dir,
                seasonEpisodes =
                  let mkEpisodeInfo :: (Int, Path Rel File) -> EpisodeInfo
                      mkEpisodeInfo (index, file) =
                        EpisodeInfo
                          { episodeSeason = if rawSeason == 0 then season else rawSeason,
                            episodeNumber = mEpisodeFromFile `orElse` EpisodeNumber index,
                            episodeSpecial = isSpecialFromFile file || rawSeason == 0,
                            episodeFileName = file
                          }
                        where
                          (mSeasonFromFile, mEpisodeFromFile) = episodeInfoFromFile file
                          rawSeason = mSeasonFromFile `orElse` season
                   in NE.sort $ mkEpisodeInfo <$> NE.zip (1 NE.:| [2 ..]) actualFiles
              }
        (_, _, _)
          | isSeriesDir ->
              Just
                SeriesDirectory
                  { seriesTitle = niceDirNameT $ dirname dir
                  }
        (_, Just actualFiles, _) ->
          Just
            MovieDirectory
              { movieTitle = movieTitleFromDir dir,
                movieYear = movieYearFromDir dir,
                movieFiles = NE.sort $ MovieInfo <$> actualFiles
              }
        _ -> Nothing
