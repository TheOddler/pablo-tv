{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Directories where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey,
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    genericParseJSON,
    genericToEncoding,
  )
import Data.Aeson.Types (FromJSONKey (..), Parser, toJSONKeyText)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Directory.Files
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Generics (Generic)
import Orphanage ()
import Samba (SmbServer (..), SmbShare (..), mkMountPath)
import System.Directory (getHomeDirectory)
import System.FilePath
import Text.Blaze (ToMarkup)
import Text.Read (Read (..))
import Util (ourAesonOptions)
import Util.Regex (expect1Int, tryRegex)
import Util.TextWithoutSeparator
import Yesod (PathPiece (..))

-- Roots

data RootDirectoryLocation
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootLocalVideos
  deriving (Eq, Ord, Generic)

unRootDirectoryLocation :: RootDirectoryLocation -> TextWithoutSeparator
unRootDirectoryLocation rd = case rd of
  RootLocalVideos -> [twsQQ|Videos|]
  RootSamba srv shr ->
    removeSeparatorsFromText . T.pack $
      concat
        [ "smb-",
          srv.unSmbServer,
          "-",
          shr.unSmbShare
        ]

rootDirectoryLocation :: (MonadFail m) => TextWithoutSeparator -> m RootDirectoryLocation
rootDirectoryLocation tws = do
  let t = unwrap tws
  if t == "Videos"
    then pure RootLocalVideos
    else case T.unpack <$> T.split (== '-') t of
      ["smb", srv, shr] ->
        pure $
          RootSamba
            (SmbServer srv)
            (SmbShare shr)
      _ -> fail $ "Unknown root directory structure: " ++ T.unpack t

rootDirectoryLocationToAbsPath :: (MonadIO m) => RootDirectoryLocation -> m FilePath
rootDirectoryLocationToAbsPath = \case
  RootSamba srv shr -> mkMountPath srv shr
  RootLocalVideos -> do
    home <- liftIO getHomeDirectory
    pure $ home </> "Videos"

instance Read RootDirectoryLocation where
  readPrec = readPrec >>= rootDirectoryLocation

instance Show RootDirectoryLocation where
  show = T.unpack . unTextWithoutSeparator . unRootDirectoryLocation

instance ToJSON RootDirectoryLocation where
  toEncoding = toEncoding . unRootDirectoryLocation

instance PathPiece RootDirectoryLocation where
  toPathPiece :: RootDirectoryLocation -> Text
  toPathPiece = unwrap . unRootDirectoryLocation

  fromPathPiece :: Text -> Maybe RootDirectoryLocation
  fromPathPiece text = textWithoutSeparator text >>= rootDirectoryLocation

parseRootDirectoryLocationFromText :: Text -> Parser RootDirectoryLocation
parseRootDirectoryLocationFromText text =
  textWithoutSeparator text >>= rootDirectoryLocation

instance FromJSON RootDirectoryLocation where
  parseJSON v = do
    (asText :: Text) <- parseJSON v
    parseRootDirectoryLocationFromText asText

instance ToJSONKey RootDirectoryLocation where
  toJSONKey = toJSONKeyText (unwrap . unRootDirectoryLocation)

instance FromJSONKey RootDirectoryLocation where
  fromJSONKey = FromJSONKeyTextParser parseRootDirectoryLocationFromText

-- Root Data

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

-- (Sub)Directories

newtype DirectoryName = DirectoryName {unDirectoryName :: TextWithoutSeparator}
  deriving newtype (Show, Eq, Ord, Read, Unwrap Text, ToJSON, FromJSON, ToJSONKey, FromJSONKey, PathPiece, ToMarkup)

-- (Sub)Directories Data

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

-- Guessing

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

-- Other helpers

splitTitleFromDir :: DirectoryName -> (Text, Text)
splitTitleFromDir dirName =
  let (title, rest) = T.breakOn "(" $ unwrap dirName
   in (T.strip title, T.strip rest)

seasonFromDir :: DirectoryName -> Maybe Int
seasonFromDir dir =
  tryRegex (unwrap dir) expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex (unwrap dir) expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex (unwrap dir) expect1Int "[Ss]eizoen ([0-9]+)"

isSeasonDir :: DirectoryName -> Bool
isSeasonDir = isJust . seasonFromDir
