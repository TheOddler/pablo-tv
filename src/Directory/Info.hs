module Directory.Info where

import Autodocodec (HasCodec (..), object, optionalFieldOrNull, optionalFieldWithDefaultWith, requiredField, stringConstCodec, (.=))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

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
