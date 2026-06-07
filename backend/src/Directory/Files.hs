{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Directory.Files where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey,
    ToJSON (..),
    ToJSONKey (..),
  )
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.UTF8 qualified as BS
import Data.HashSet qualified as Set
import Data.List.Extra (dropPrefix, dropSuffix, lower, stripPrefix, uncons)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Exts (sortWith)
import JSON (HasJSONPrefix (..), deriveJSONPrefixed)
import Orphanage ()
import SafeIO (SafeIO, randomFileNameSuffix)
import System.FilePath (takeBaseName, takeExtension)
import Util (safeMinimumOn)
import Util.Regex
import Util.TextWithoutSeparator

-- Videos

newtype VideoFileName = VideoFileName {unVideoFileName :: TextWithoutSeparator}
  deriving newtype (Show, Eq, Ord, Unwrap Text, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data VideoFileData = VideoFileData
  { videoFileAdded :: UTCTime,
    videoFileWatched :: Maybe UTCTime
  }
  deriving (Eq, Show)

instance HasJSONPrefix VideoFileData where
  type JSONPrefix VideoFileData = "videoFile"

deriveJSONPrefixed ''VideoFileData

-- Images

data Image
  = ImageOnDisk ImageFileName CachedImageFileName
  | ImageFromWeb CachedImageFileName
  deriving (Show, Eq)

instance ToJSON Image where
  toJSON = \case
    ImageOnDisk name cached ->
      Aeson.object ["name" Aeson..= name, "cached" Aeson..= cached]
    ImageFromWeb cached ->
      Aeson.object ["cached" Aeson..= cached]
  toEncoding = \case
    ImageOnDisk name cached ->
      Aeson.pairs ("name" Aeson..= name <> "cached" Aeson..= cached)
    ImageFromWeb cached ->
      Aeson.pairs ("cached" Aeson..= cached)

instance FromJSON Image where
  parseJSON = Aeson.withObject "Image" $ \o -> do
    cachedName <- o Aeson..: "cached"
    mName <- o Aeson..:? "name"
    case mName of
      Just name -> pure $ ImageOnDisk name cachedName
      Nothing -> pure $ ImageFromWeb cachedName

newtype ImageFileName = ImageFileName TextWithoutSeparator
  deriving newtype (Eq, Show, Unwrap Text, ToJSON, FromJSON)

bestImageFile :: [ImageFileName] -> Maybe ImageFileName
bestImageFile = safeMinimumOn $ \fileName ->
  case lower $ takeBaseName $ T.unpack $ unwrap fileName of
    "poster" -> 0 :: Int
    "cover" -> 1
    _ -> 100

newtype CachedImageFileName = CachedImageFileName TextWithoutSeparator
  deriving newtype (Eq, Show, Unwrap Text, ToJSON, FromJSON)

mkCachedImageFileName :: (SafeIO m) => [TextWithoutSeparator] -> Either ImageFileName BS.ByteString -> m CachedImageFileName
mkCachedImageFileName path originalNameOrContentType = do
  suffix <- randomFileNameSuffix
  let name :: Maybe TextWithoutSeparator
      ext :: TextWithoutSeparator
      (name, ext) = case originalNameOrContentType of
        Left (ImageFileName originalName) ->
          let (n, e) = splitExtension originalName
           in (Just n, e)
        Right contentType ->
          ( Nothing,
            case stripPrefix "image/" $ BS.toString contentType of
              Just ext' -> [twsQQ|.|] <> removeSeparatorsFromText (T.pack ext')
              Nothing -> [twsQQ|.jpg|]
          )
      fullName = intercalate [twsQQ|-|] $ path ++ maybeToList name ++ [removeSeparatorsFromText $ T.pack suffix]
  pure . CachedImageFileName $ fullName <> ext

cachedImageContentType :: CachedImageFileName -> BS.ByteString
cachedImageContentType (CachedImageFileName imgName) =
  case takeExtension $ T.unpack $ unwrap imgName of
    "" -> "image/jpg"
    ext ->
      let cleanedExt = lower $
            case ext of
              '.' : e -> e
              e -> e
       in "image/" <> BS8.pack cleanedExt

-- Nicely collapsing file names

data NiceVideoFileNames = NiceVideoFileNames
  { commonPrefix :: Text,
    uniqueMiddles :: [Text],
    commonSuffix :: Text
  }
  deriving (Eq, Show)

niceFileNames :: [VideoFileName] -> NiceVideoFileNames
niceFileNames =
  \case
    [] -> NiceVideoFileNames "" [] ""
    [name] -> NiceVideoFileNames "" [T.unwords $ splitName name] ""
    names ->
      let splitNames = splitName <$> names

          commonPrefix' = findCommonPrefix splitNames
          commonPrefix = T.unwords commonPrefix'

          namesWithoutPrefixes = dropPrefix commonPrefix' <$> splitNames

          commonSuffix' = findCommonSuffix namesWithoutPrefixes
          commonSuffix = T.unwords commonSuffix'

          uniqueMiddles' = dropSuffix commonSuffix' <$> namesWithoutPrefixes
          uniqueMiddles = T.unwords <$> uniqueMiddles'
       in NiceVideoFileNames commonPrefix uniqueMiddles commonSuffix
  where
    delims :: Set Char
    delims = [' ', '.']

    dropVideoFileExts :: VideoFileName -> Text
    dropVideoFileExts name =
      let go [] = unwrap name
          go (ext : rest) =
            case T.stripSuffix ext (unwrap name) of
              Nothing -> go rest
              Just n -> n
       in go $ unwrap <$> Set.toList videoFileExts

    splitName :: VideoFileName -> [Text]
    splitName videoFile = T.split (`elem` delims) (dropVideoFileExts videoFile)

    allJusts :: [Maybe a] -> Maybe [a]
    allJusts [] = Just []
    allJusts (Nothing : _) = Nothing
    allJusts (Just x : xs) =
      case allJusts xs of
        Nothing -> Nothing
        Just aj -> Just $ x : aj

    findCommonPrefix :: (Eq a) => [[a]] -> [a]
    findCommonPrefix xs = case allJusts (uncons <$> xs) of
      Nothing -> []
      Just ((head', tail') : rest)
        | all ((== head') . fst) rest ->
            head' : findCommonPrefix (tail' : map snd rest)
      _ -> []

    findCommonSuffix :: (Eq a) => [[a]] -> [a]
    findCommonSuffix = reverse . findCommonPrefix . map reverse

-- Other helpers

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
