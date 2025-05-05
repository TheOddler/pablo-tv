{-# LANGUAGE TemplateHaskell #-}

module Directory.Files where

import Autodocodec (HasCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Control.Exception (IOException)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Yaml qualified as Yaml
import Path (Abs, File, Path, Rel, fileExtension, filename, mkRelFile)
import SaferIO (FSRead (..))
import System.Posix (FileStatus)
import TVDB (TVDBImageUrl)
import Yesod (ContentType)

infoFileName :: Path Rel File
infoFileName = $(mkRelFile "info.yaml")

watchedFileName :: Path Rel File
watchedFileName = $(mkRelFile "watched.yaml")

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

-- | Extra info needed to manage the info.yaml and watched.yaml files.
-- This keeps info that we can use to know whether we need to save updated info to disk.
data SpecialFile a
  = FileDoesNotExist
  | FileRead a
  | -- | If we update the file in memory mark it as dirty so we know we need to write it to disk.
    FileDirty a
  | -- | If a file existed but parsing failed
    FileReadFail BS8.ByteString Yaml.ParseException
  | -- | If we got an error while trying to read the file
    FileReadError IOException

readSpecialFile :: (FSRead m, HasCodec a) => Path Abs File -> m (SpecialFile a)
readSpecialFile path = do
  contentOrErr <- readFileBSSafe path
  pure $ case contentOrErr of
    Left err -> FileReadError err
    Right content ->
      case eitherDecodeYamlViaCodec content of
        Right info -> FileRead info
        Left err -> FileReadFail content err

type InMemoryImage = (ContentType, BS.ByteString)

data Image
  = ImageOnDisk (Path Abs File)
  | ImageOnWeb TVDBImageUrl
  | ImageDownloaded InMemoryImage

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
