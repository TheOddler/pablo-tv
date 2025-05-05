module Directory.Info where

import Autodocodec (HasCodec (..), object, optionalFieldOrNull, optionalFieldWithDefaultWith, requiredField, stringConstCodec, (.=))
import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Directory.Files (VideoFile (..))
import GHC.Data.Maybe (firstJusts, orElse)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, fromRelDir, fromRelFile, splitExtension)
import Regex (expect1Int, expect2Ints, expect3Ints, tryRegex)
import System.FilePath (dropTrailingPathSeparator)

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

-- Guessing Info

guessInfo :: Path Abs Dir -> [VideoFile] -> [Path Abs Dir] -> Maybe DirectoryInfo
guessInfo path videoFiles subDirs = do
  kind <- guessDirectoryKind videoFiles subDirs
  Just $
    DirectoryInfo
      { directoryInfoKind = kind,
        directoryInfoTitle = titleFromDir path,
        directoryInfoYear =
          yearFromDir path
            <|> yearFromFiles (videoFilePath <$> videoFiles),
        directoryInfoDescription = Nothing,
        directoryInfoImdb = Nothing,
        directoryInfoTvdb = Nothing,
        directoryInfoTmdb = Nothing,
        directoryInfoForceUpdate = Nothing
      }

guessDirectoryKind :: [VideoFile] -> [Path Abs Dir] -> Maybe DirectoryKind
guessDirectoryKind videoFiles subDirs =
  let isSeriesFromFiles = isJust . seasonFromFiles $ videoFilePath <$> videoFiles
      isSeriesFromSubDirs = any (isJust . seasonFromDir) subDirs
      isSeries = isSeriesFromFiles || isSeriesFromSubDirs
   in case (NE.nonEmpty videoFiles, NE.nonEmpty subDirs, isSeries) of
        -- If there are no video files nor subDirs, then this is just a passthrough dir
        (Nothing, Nothing, True) -> Nothing
        (Nothing, Nothing, False) -> Nothing
        -- If there are video files, check if it's a series
        (Just _, Nothing, True) -> Just DirectoryKindSeries
        (Just _, Nothing, False) -> Just DirectoryKindMovie
        -- If there are subDirs that indicate a series, it's that
        (Nothing, Just _, True) -> Just DirectoryKindSeries
        -- If there are only subDirs but they don't indicate a series, this folder is just a passthrough dir
        (Nothing, Just _, False) -> Nothing
        -- If there are video files and subDirs, use the series indicator
        (Just _, Just _, True) -> Just DirectoryKindSeries
        (Just _, Just _, False) -> Just DirectoryKindMovie

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
    -- Take the season that occurs most
    mSeason = NE.head <$> listToMaybe (sortOn NE.length $ NE.group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

    seasonFromFile :: Path a File -> Maybe Int
    seasonFromFile file =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

titleFromDir :: Path a Dir -> Text
titleFromDir folder = T.strip . fst $ T.breakOn "(" (niceDirNameT folder)

niceDirNameT :: Path a Dir -> Text
niceDirNameT = T.pack . dropTrailingPathSeparator . fromRelDir . dirname

niceFileNameT :: Path a File -> Text
niceFileNameT file =
  let withoutExt = (fst <$> splitExtension file) `orElse` file
      name = fromRelFile $ filename withoutExt
   in T.replace "." " " $ T.pack name

yearFromPath :: Path Rel x -> Maybe Int
yearFromPath path =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex path expect2Ints "((19|20)[0-9][0-9])"

yearFromDir :: Path a Dir -> Maybe Int
yearFromDir = yearFromPath . dirname

yearFromFiles :: [Path a File] -> Maybe Int
yearFromFiles files = firstJusts (yearFromPath . filename <$> files)

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
