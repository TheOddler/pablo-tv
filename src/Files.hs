module Files where

import Control.Applicative ((<|>))
import Data.Text (Text, breakOn, isPrefixOf, strip, unpack)
import Data.Text qualified as T
import GHC.Utils.Misc (fstOf3)
import Text.Read (readMaybe)
import Text.Regex.TDFA

data EpisodeInfo = EpisodeInfo
  { episodeSeason :: Int,
    episodeNumber :: EpisodeNumber,
    episodeSpecial :: Bool
  }
  deriving (Eq, Show)

data EpisodeNumber
  = EpisodeNumber Int
  | EpisodeNumberDouble Int Int
  deriving (Eq, Show)

data MovieInfo = MovieInfo
  { movieTitle :: Text,
    movieYear :: Maybe Int
  }
  deriving (Eq, Show)

data FileInfo
  = FileEpisode EpisodeInfo
  | FileMovie MovieInfo
  deriving (Eq, Show)

parseFileName :: Int -> Int -> [Text] -> FileInfo
parseFileName indexInDir fileCount segments =
  let (folder, file) = case reverse segments of
        [] -> ("", "")
        [x] -> ("", x)
        (fileN : folderN : _) -> (folderN, fileN)

      readInt :: Text -> Maybe Int
      readInt = readMaybe . unpack . strip

      seasonFromFolder :: Maybe Int
      seasonFromFolder
        | "Season" `isPrefixOf` folder = readInt $ T.drop 6 folder
        | "Series" `isPrefixOf` folder = readInt $ T.drop 6 folder
        | otherwise = Nothing

      tryRegexFolder :: ([Text] -> Maybe a) -> Text -> Maybe a
      tryRegexFolder f regex =
        let res :: (Text, Text, Text, [Text])
            res = folder =~ regex
            (_, _, _, matches) = res
         in f matches

      tryRegex :: ([Text] -> Maybe a) -> Text -> Maybe a
      tryRegex f regex =
        let res :: (Text, Text, Text, [Text])
            res = file =~ regex
            (_, _, _, matches) = res
         in f matches

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

      seasonFromFile :: Maybe Int
      seasonFromFile =
        tryRegex expect1Int "[Ss]eason ([0-9]+)"
          <|> tryRegex expect1Int "[Ss]eries ([0-9]+)"

      doubleEpisodeFromFile :: Maybe (Int, Int, Int)
      doubleEpisodeFromFile =
        tryRegex expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      episodeInfoFromFile :: Maybe (Int, Int)
      episodeInfoFromFile =
        tryRegex expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex expect2Ints "([0-9]+)[Xx]([0-9]+)"

      episodeFromFile :: Maybe Int
      episodeFromFile =
        tryRegex expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex expect1Int "[Aa]flevering ([0-9]+)"
          <|> tryRegex expect1Int "[Ss]pecial ([0-9]+)"

      season =
        seasonFromFolder
          <|> fstOf3 <$> doubleEpisodeFromFile
          <|> fst <$> episodeInfoFromFile
          <|> seasonFromFile

      isSpecial :: Bool
      isSpecial =
        (fstOf3 <$> doubleEpisodeFromFile <|> fst <$> episodeInfoFromFile) == Just 0
          || file =~ ("[Ss]pecial" :: Text)

      (confidentEpisode, episode) = case (doubleEpisodeFromFile, episodeInfoFromFile, episodeFromFile) of
        (Just (_, a, b), _, _) -> (True, EpisodeNumberDouble a b)
        (Nothing, Just (_, a), _) -> (True, EpisodeNumber a)
        (Nothing, Nothing, Just a) -> (True, EpisodeNumber a)
        (Nothing, Nothing, Nothing) -> (False, EpisodeNumber (indexInDir + 1))

      movieYear =
        let yearRegex = "((19|20)[0-9][0-9])"
         in fst
              <$> ( tryRegexFolder expect2Ints yearRegex
                      <|> tryRegex expect2Ints yearRegex
                  )

      movieTitle = case folder of
        "" -> strip . fst $ breakOn "(" file
        _ -> strip . fst $ breakOn "(" folder
   in case (season, confidentEpisode) of
        (Just s, _) -> FileEpisode $ EpisodeInfo s episode isSpecial
        (Nothing, True) -> FileEpisode $ EpisodeInfo 1 episode isSpecial
        -- This next rule is a bit silly, might remove it
        -- Maybe it makes more sense to not try to parse movies and series from the same folder?
        (Nothing, _) | fileCount >= 2 -> FileEpisode $ EpisodeInfo 1 episode True
        (Nothing, False) -> FileMovie $ MovieInfo movieTitle movieYear
