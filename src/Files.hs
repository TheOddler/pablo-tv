module Files where

import Data.Text (Text)
import Text.Regex (matchRegex, mkRegex)

data EpisodeInfo = EpisodeInfo
  { episodeTitle :: Maybe Text,
    episodeNumber :: Int,
    episodeSeason :: Int
  }
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

parseFileName :: Int -> [Text] -> FileInfo
parseFileName _indexInDir segments =
  let (_folderName, fileName) =
        case reverse segments of
          [] -> ("", "")
          [x] -> ("", x)
          (fileN : folderN : _) -> (folderN, fileN)
   in FileMovie $ MovieInfo fileName Nothing
