{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Files where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey,
    ToJSON (..),
    ToJSONKey (..),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.UTF8 qualified as BS
import Data.HashSet qualified as Set
import Data.List.Extra (lower, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import Orphanage ()
import SafeConvert (bsToBase64Text)
import System.FilePath (takeBaseName, takeExtension)
import Util (ourAesonOptions, safeMinimumOn)
import Util.Regex
import Util.TextWithoutSeparator
import Yesod (ContentType, ToContent, typeJpeg)

-- Videos

newtype VideoFileName = VideoFileName {unVideoFileName :: TextWithoutSeparator}
  deriving newtype (Show, Eq, Ord, Unwrap Text, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data VideoFileData = VideoFileData
  { videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show)

instance ToJSON VideoFileData where
  toJSON = genericToJSON ourAesonOptions
  toEncoding = genericToEncoding ourAesonOptions

instance FromJSON VideoFileData where
  parseJSON = genericParseJSON ourAesonOptions

-- Images

newtype ImageFileName = ImageFileName TextWithoutSeparator
  deriving newtype (Eq, Show, Unwrap Text, ToJSON, FromJSON)

getImageContentType :: ImageFileName -> ContentType
getImageContentType (ImageFileName imgName) =
  case takeExtension $ T.unpack $ unwrap imgName of
    "" -> typeJpeg
    ext ->
      let cleanedExt = lower $
            case ext of
              '.' : e -> e
              e -> e
       in "image/" <> BS8.pack cleanedExt

-- | We assume this is a properly formatted content type, something like "image/jpg".
-- If it is not, we silently return some default or possibly a weird filename
imageFileNameForContentType :: ContentType -> ImageFileName
imageFileNameForContentType ct = ImageFileName $
  case stripPrefix "image/" $ BS.toString ct of
    Just ext -> [twsQQ|poster.|] <> removeSeparatorsFromText (T.pack ext)
    Nothing -> [twsQQ|poster.jpg|]

newtype ImageFileData = ImageFileData {unImageFileData :: BS.ByteString}
  deriving newtype (ToContent, Eq, Show)

instance ToJSON ImageFileData where
  toJSON imgData = toJSON $ bsToBase64Text imgData.unImageFileData
  toEncoding imgData = toEncoding $ bsToBase64Text imgData.unImageFileData

instance FromJSON ImageFileData where
  parseJSON jsonValue = do
    t <- parseJSON jsonValue
    pure $ ImageFileData $ B64.decodeLenient $ TE.encodeUtf8 t

bestImageFile :: [ImageFileName] -> Maybe ImageFileName
bestImageFile = safeMinimumOn $ \fileName ->
  case lower $ takeBaseName $ T.unpack $ unwrap fileName of
    "poster" -> 0 :: Int
    "cover" -> 1
    _ -> 100

-- Other helpers

niceFileNameT :: VideoFileName -> Text
niceFileNameT file =
  T.replace "." " " $ T.pack $ takeBaseName $ T.unpack $ unwrap file

seasonFromFiles :: [VideoFileName] -> Maybe Int
seasonFromFiles fileNames =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ NE.group (mapMaybe seasonFromFile fileNames))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) fileNames

    seasonFromFile :: VideoFileName -> Maybe Int
    seasonFromFile file =
      tryRegex (unwrap file) expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex (unwrap file) expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex (unwrap file) expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: VideoFileName -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile file =
  let double :: Maybe (Int, Int, Int)
      double =
        tryRegex (unwrap file) expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      seasonAndEp :: Maybe (Int, Int)
      seasonAndEp =
        tryRegex (unwrap file) expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex (unwrap file) expect2Ints "([0-9]+)[Xx]([0-9]+)"

      epOnly :: Maybe Int
      epOnly =
        tryRegex (unwrap file) expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex (unwrap file) expect1Int "[Aa]flevering ([0-9]+)"
   in case double of
        Just (s, a, b) -> (Just s, Just $ Right (a, b))
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ Left a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ Left a)
            Nothing -> (Nothing, Nothing)

hasVideoFileExt :: TextWithoutSeparator -> Bool
hasVideoFileExt t = videoFileExts `anyIsSuffixOf` t

videoFileExts :: Set.HashSet TextWithoutSeparator
videoFileExts =
  [ [twsQQ|.avi|],
    [twsQQ|.flv|],
    [twsQQ|.m4v|],
    [twsQQ|.mkv|],
    [twsQQ|.mov|],
    [twsQQ|.mp4|],
    [twsQQ|.mpeg|],
    [twsQQ|.mpg|],
    [twsQQ|.webm|],
    [twsQQ|.wmv|]
  ]

hasImageFileExt :: TextWithoutSeparator -> Bool
hasImageFileExt t = imageFileExts `anyIsSuffixOf` t

imageFileExts :: Set.HashSet TextWithoutSeparator
imageFileExts =
  [ [twsQQ|.bmp|],
    [twsQQ|.gif|],
    [twsQQ|.heic|],
    [twsQQ|.heif|],
    [twsQQ|.jpeg|],
    [twsQQ|.jpg|],
    [twsQQ|.png|],
    [twsQQ|.svg|],
    [twsQQ|.tif|],
    [twsQQ|.tiff|],
    [twsQQ|.webp|]
  ]

-- | For non-video, non-image ext.
-- This is used to improve guesses on what are files and what are directories.
hasCommonFileExt :: TextWithoutSeparator -> Bool
hasCommonFileExt t = commonFileExts `anyIsSuffixOf` t

commonFileExts :: Set.HashSet TextWithoutSeparator
commonFileExts =
  [ [twsQQ|.7z|],
    [twsQQ|.aac|],
    [twsQQ|.aes|],
    [twsQQ|.apk|],
    [twsQQ|.bat|],
    [twsQQ|.csv|],
    [twsQQ|.db|],
    [twsQQ|.db3-shm|],
    [twsQQ|.db3-wal|],
    [twsQQ|.db3|],
    [twsQQ|.dmg|],
    [twsQQ|.doc|],
    [twsQQ|.docx|],
    [twsQQ|.exe|],
    [twsQQ|.flac|],
    [twsQQ|.gz|],
    [twsQQ|.idx|],
    [twsQQ|.img|],
    [twsQQ|.iso|],
    [twsQQ|.mp3|],
    [twsQQ|.msi|],
    [twsQQ|.odp|],
    [twsQQ|.ods|],
    [twsQQ|.odt|],
    [twsQQ|.ogg|],
    [twsQQ|.pdf|],
    [twsQQ|.ppt|],
    [twsQQ|.pptx|],
    [twsQQ|.rar|],
    [twsQQ|.rtf|],
    [twsQQ|.sh|],
    [twsQQ|.srt|],
    [twsQQ|.sub|],
    [twsQQ|.tar.gz|],
    [twsQQ|.tar|],
    [twsQQ|.txt|],
    [twsQQ|.wav|],
    [twsQQ|.xls|],
    [twsQQ|.xlsx|],
    [twsQQ|.yaml|],
    [twsQQ|.zip|]
  ]
