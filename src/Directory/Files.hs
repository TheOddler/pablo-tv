{-# LANGUAGE TemplateHaskell #-}

module Directory.Files where

import Autodocodec (HasCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Control.Exception (IOException)
import Data.ByteString qualified as BS
import Data.Yaml qualified as Yaml
import Path (Abs, File, Path, Rel, fileExtension, filename, mkRelFile)
import SaferIO (FSRead (..))
import System.Posix (FileStatus)
import Yesod (ContentType)

infoFileName :: Path Rel File
infoFileName = $(mkRelFile "info.yaml")

watchedFileName :: Path Rel File
watchedFileName = $(mkRelFile "watched.yaml")

-- | We support images with any name, but when we download them from the internet without a proper name this is the name we'll give it
posterFileNameDefault :: Path Rel File
posterFileNameDefault = $(mkRelFile "watched.yaml")

-- | The extensions we consider video files.
-- Must include the `.` as that's what `takeExtension` gives us so easier to use that way.
videoExtensions :: [String]
videoExtensions = [".mp4", ".mkv", ".avi", ".webm"]

imageExtensions :: [String]
imageExtensions = [".jpg", ".jpeg", ".png", ".gif"]

data VideoFile = VideoFile
  { videoFilePath :: Path Abs File,
    videoFileStatus :: FileStatus
  }

newtype OtherFile = OtherFile
  { otherFilePath :: Path Abs File
  }

data FileReadByCodecError
  = FileReadError IOException
  | FileParseError BS.ByteString Yaml.ParseException

readFileByCodec :: (FSRead m, HasCodec a) => Path Abs File -> m (Either FileReadByCodecError a)
readFileByCodec path = do
  contentOrErr <- readFileBSSafe path
  pure $ case contentOrErr of
    Left err -> Left $ FileReadError err
    Right content ->
      case eitherDecodeYamlViaCodec content of
        Right info -> Right info
        Left err -> Left $ FileParseError content err

newtype ImageFile = ImageFile
  { imageFilePath :: Path Abs File
  }

data ImageInMemory = ImageInMemory ContentType BS.ByteString

-- Helpers

fileNameIs :: Path Rel File -> Path a File -> Bool
fileNameIs n = (== n) . filename

fileNameIsOneOf :: [Path Rel File] -> Path a File -> Bool
fileNameIsOneOf ns p = filename p `elem` ns

extensionIsOneOf :: [String] -> Path a File -> Bool
extensionIsOneOf exts path =
  case fileExtension path of
    Nothing -> False
    Just ext -> ext `elem` exts

isVideoFile :: Path a File -> Bool
isVideoFile = extensionIsOneOf videoExtensions

isImageFile :: Path a File -> Bool
isImageFile = extensionIsOneOf imageExtensions
