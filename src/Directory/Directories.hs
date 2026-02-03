{-# LANGUAGE DerivingStrategies #-}

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
    genericToJSON,
  )
import Data.Aeson.Types (FromJSONKey (..), toJSONKeyText)
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
import Util (ourAesonOptionsPrefix)
import Util.DirPath (Abs, DirPath (..), Rel, absPath, relPath, unDirPath)
import Util.Regex (expect1Int, tryRegex)
import Util.TextWithoutSeparator
import Yesod (PathPiece (..))

-- Roots

type RootDirectories = Map.Map RootDirectoryLocation RootDirectoryData

data RootDirectoryLocation
  = RootSamba Samba.SmbServer Samba.SmbShare
  | RootRelToHome (DirPath Rel)
  | RootAbsPath (DirPath Abs)
  deriving (Eq, Ord, Generic)

rootDirectoryLocationToAbsPath :: (MonadIO m) => RootDirectoryLocation -> m FilePath
rootDirectoryLocationToAbsPath = \case
  RootSamba srv shr -> mkMountPath srv shr
  RootRelToHome r -> do
    home <- liftIO getHomeDirectory
    pure $ home </> T.unpack (unDirPath r)
  RootAbsPath a -> pure $ T.unpack $ unDirPath a

rootDirectoryLocationToText :: RootDirectoryLocation -> Text
rootDirectoryLocationToText rd = case rd of
  RootRelToHome r -> unDirPath r
  RootSamba srv shr ->
    T.pack $
      concat
        [ "smb-",
          srv.unSmbServer,
          "-",
          shr.unSmbShare
        ]
  RootAbsPath a -> unDirPath a

instance Show RootDirectoryLocation where
  show = T.unpack . rootDirectoryLocationToText

instance ToJSON RootDirectoryLocation where
  toJSON = toJSON . rootDirectoryLocationToText
  toEncoding = toEncoding . rootDirectoryLocationToText

instance ToJSONKey RootDirectoryLocation where
  toJSONKey = toJSONKeyText rootDirectoryLocationToText

parseRootDirectoryLocationFromText :: (MonadFail m) => Text -> m RootDirectoryLocation
parseRootDirectoryLocationFromText t = do
  case T.unpack <$> T.split (== '-') t of
    ["smb", srv, shr] ->
      pure $
        RootSamba
          (SmbServer srv)
          (SmbShare shr)
    _ -> do
      let attempts :: [Maybe RootDirectoryLocation]
          attempts =
            [ RootAbsPath <$> absPath t,
              RootRelToHome <$> relPath t
            ]
      case firstJusts attempts of
        Nothing -> fail $ "Couldn't parse root directory from " ++ show t
        Just root -> pure root

instance Read RootDirectoryLocation where
  readPrec = readPrec >>= parseRootDirectoryLocationFromText

instance FromJSON RootDirectoryLocation where
  parseJSON v = do
    (asText :: Text) <- parseJSON v
    parseRootDirectoryLocationFromText asText

instance FromJSONKey RootDirectoryLocation where
  fromJSONKey = FromJSONKeyTextParser parseRootDirectoryLocationFromText

-- Root Data

data RootDirectoryData = RootDirectoryData
  { rootDirectorySubDirs :: Map.Map DirectoryName DirectoryData,
    rootDirectoryVideoFiles :: Map.Map VideoFileName VideoFileData
  }
  deriving (Generic, Show, Eq)

instance ToJSON RootDirectoryData where
  toJSON = genericToJSON $ ourAesonOptionsPrefix "rootDirectory"
  toEncoding = genericToEncoding $ ourAesonOptionsPrefix "rootDirectory"

instance FromJSON RootDirectoryData where
  parseJSON = genericParseJSON $ ourAesonOptionsPrefix "rootDirectory"

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
  toJSON = genericToJSON $ ourAesonOptionsPrefix "directory"
  toEncoding = genericToEncoding $ ourAesonOptionsPrefix "directory"

instance FromJSON DirectoryData where
  parseJSON = genericParseJSON $ ourAesonOptionsPrefix "directory"

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
