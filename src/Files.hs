module Files
  ( DirectoryInfo (..),
    EpisodeInfo (..),
    EpisodeNumber (..),
    MovieInfo (..),
    parseDirectory,
  )
where

import Control.Applicative ((<|>))
import Data.List (sort)
import Data.List.NonEmpty (group)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text, breakOn, strip, unpack)
import Data.Text qualified as T
import GHC.Data.Maybe (listToMaybe, orElse)
import GHC.Exts (sortWith)
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
    episodeFileName :: Text
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
  compare (EpisodeNumber a) (EpisodeNumberDouble c d) = compare a c <> compare a d
  compare (EpisodeNumberDouble c d) (EpisodeNumber a) = compare c a <> compare d a
  compare (EpisodeNumber a) (EpisodeNumber b) = compare a b
  compare (EpisodeNumberDouble a b) (EpisodeNumberDouble c d) = compare a c <> compare b d

newtype MovieInfo = MovieInfo
  { movieFileName :: Text
  }
  deriving (Eq, Show, Ord)

data DirectoryInfo
  = SeriesDirectory
      { seriesTitle :: Text
      }
  | SeasonDirectory
      { seasonSeriesTitle :: Text,
        seasonEpisodes :: [EpisodeInfo]
      }
  | MovieDirectory
      { movieTitle :: Text,
        movieYear :: Maybe Int,
        movieFiles :: [MovieInfo]
      }
  deriving (Eq, Show, Ord)

-- Some helpers

readInt :: Text -> Maybe Int
readInt = readMaybe . unpack . strip

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

-- Getting the info

seasonFromFolder :: Text -> Maybe Int
seasonFromFolder folder =
  tryRegex folder expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex folder expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex folder expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: [Text] -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

    seasonFromFile :: Text -> Maybe Int
    seasonFromFile file =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: Text -> (Maybe Int, Maybe EpisodeNumber)
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

isSpecialFromFile :: Text -> Bool
isSpecialFromFile file =
  file =~ ("[Ss]pecial" :: Text)
    || file =~ ("[Ss]0+[Ee][0-9]+" :: Text)
    || file =~ ("0+[Xx][0-9]+" :: Text)

movieYearFromFolder :: Text -> Maybe Int
movieYearFromFolder folder =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex folder expect2Ints "((19|20)[0-9][0-9])"

movieTitleFromFolder :: Text -> Text
movieTitleFromFolder folder = strip . fst $ breakOn "(" folder

parseDirectory :: Text -> Text -> [Text] -> [Text] -> DirectoryInfo
parseDirectory parentFolder folder files directories =
  let videoFiles = sort $ filter isVideoFile files
      isVideoFile file = any (`T.isSuffixOf` file) [".mp4", ".mkv", ".avi", ".webm"]
      isSeriesFolder = any (isJust . seasonFromFolder) directories
      mSeasonFromFolder = seasonFromFolder folder
      mSeasonFromFiles = seasonFromFiles videoFiles
   in case mSeasonFromFolder <|> mSeasonFromFiles of
        Just season ->
          SeasonDirectory
            { seasonSeriesTitle =
                if isJust mSeasonFromFolder && parentFolder /= ""
                  then parentFolder
                  else folder,
              seasonEpisodes =
                sort
                  [ EpisodeInfo
                      { episodeSeason = if rawSeason == 0 then season else rawSeason,
                        episodeNumber = mEpisodeFromFile `orElse` index,
                        episodeSpecial = isSpecialFromFile file || rawSeason == 0,
                        episodeFileName = file
                      }
                    | (index, file) <- zip (EpisodeNumber <$> [1 ..]) videoFiles,
                      (mSeasonFromFile, mEpisodeFromFile) <- [episodeInfoFromFile file],
                      rawSeason <- [mSeasonFromFile `orElse` season]
                  ]
            }
        Nothing ->
          if isSeriesFolder
            then
              SeriesDirectory
                { seriesTitle = folder
                }
            else
              MovieDirectory
                { movieTitle = movieTitleFromFolder folder,
                  movieYear = movieYearFromFolder folder,
                  movieFiles = sort $ MovieInfo <$> videoFiles
                }
