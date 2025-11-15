module Directory.Paths where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    withText,
  )
import Data.Aeson.Types (parseFail)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Directory.Directories
import Directory.Files
import GHC.Generics (Generic)
import Orphanage ()
import Samba qualified
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension)
import Util.TextWithoutSeparator
import Yesod (MonadIO (..))

-- Directory paths

data DirectoryPath = DirectoryPath
  { directoryPathRoot :: RootDirectoryLocation,
    directoryPathNames :: [DirectoryName]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DirectoryPath where
  toEncoding (DirectoryPath root names) =
    toEncoding . unsplitSeparatedText $
      unRootDirectoryLocation root : (unDirectoryName <$> names)

instance FromJSON DirectoryPath where
  parseJSON = withText "DirectoryPath" $ \text ->
    case parseRootAndNames $ splitAtSeparatorNE text of
      Just (root, names) -> pure $ DirectoryPath root names
      Nothing -> parseFail $ "Couldn't parse DirectoryPath: " ++ T.unpack text

parseRootAndNames :: (MonadFail m) => NE.NonEmpty TextWithoutSeparator -> m (RootDirectoryLocation, [DirectoryName])
parseRootAndNames (mRoot NE.:| names) = do
  root <- rootDirectoryLocation mRoot
  pure (root, DirectoryName <$> names)

addSubDir :: DirectoryPath -> DirectoryName -> DirectoryPath
addSubDir (DirectoryPath root names) newName = DirectoryPath root $ names ++ [newName]

directoryPathToAbsPath :: (MonadIO m) => DirectoryPath -> m FilePath
directoryPathToAbsPath (DirectoryPath root dirNames) = do
  rootAbsPath <- case root of
    RootSamba srv shr -> Samba.mkMountPath srv shr
    RootLocalVideos -> do
      home <- liftIO getHomeDirectory
      pure $ home ++ "/Videos"
  pure $ intercalate "/" $ rootAbsPath : (T.unpack . unwrap <$> dirNames)

-- Video File Paths

data VideoFilePath = VideoFilePath
  { videoFilePathRoot :: RootDirectoryLocation,
    videoFilePathNames :: [DirectoryName],
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq, Generic)

instance ToJSON VideoFilePath where
  toEncoding (VideoFilePath root dirNames videoName) =
    toEncoding . unsplitSeparatedText $
      unRootDirectoryLocation root
        : (unDirectoryName <$> dirNames)
        ++ [videoName.unVideoFileName]

instance FromJSON VideoFilePath where
  parseJSON = withText "VideoFilePath" $ \text -> do
    let pieces = splitAtSeparatorNE text
        fileNameT = NE.last pieces
        fileName = VideoFileName fileNameT
        isVideoFile :: Bool
        isVideoFile = hasVideoFileExt fileNameT
        dirPathPieces = NE.nonEmpty $ NE.init pieces
    if isVideoFile
      then case parseRootAndNames <$> dirPathPieces of
        Just (Right (root, names)) -> pure $ VideoFilePath root names fileName
        Just (Left err) -> parseFail $ "Couldn't parse VideoFilePath: " ++ err
        Nothing -> parseFail "Couldn't parse VideoFilePath, no directory found in path."
      else parseFail "Not a video file, wrong extension."

videoFilePath :: DirectoryPath -> VideoFileName -> VideoFilePath
videoFilePath dir videoName =
  VideoFilePath
    { videoFilePathRoot = dir.directoryPathRoot,
      videoFilePathNames = dir.directoryPathNames,
      videoFilePathName = videoName
    }

videoFilePathToAbsPath :: VideoFilePath -> IO FilePath
videoFilePathToAbsPath (VideoFilePath root dirNames fileName) = do
  dirAbsPath <- directoryPathToAbsPath $ DirectoryPath root dirNames
  pure $ dirAbsPath ++ "/" ++ T.unpack (unwrap fileName)

-- Guessing path types

-- | Reading the file info of a path is rather slow, so we want to minimise the ones we read it for.
-- So instead of reading it for every path, we first do a guess what the path is, and read it for dirs only.
-- There's also some paths we ignore, regardless of wether they are files or dirs.
data LikelyPathType
  = LikelyDir DirectoryName
  | LikelyVideoFile VideoFileName
  | LikelyImageFile ImageFileName
  | LikelyOther TextWithoutSeparator

-- | This expects a string that's the result of a `listDirectory` call
guessType :: TextWithoutSeparator -> LikelyPathType
guessType tws = case str of
  "" -> LikelyOther tws
  '.' : _ -> LikelyOther tws
  _ -> case takeExtension str of
    _ | hasVideoFileExt tws -> LikelyVideoFile $ VideoFileName tws
    _ | hasImageFileExt tws -> LikelyImageFile $ ImageFileName tws
    _ | hasCommonFileExt tws -> LikelyOther tws
    _ -> LikelyDir $ DirectoryName tws
  where
    str = T.unpack tws.unTextWithoutSeparator

partitionPathTypes :: [LikelyPathType] -> ([DirectoryName], [VideoFileName], [ImageFileName], [TextWithoutSeparator])
partitionPathTypes =
  foldr
    ( \guess (dirs, vids, imgs, oths) -> case guess of
        LikelyDir d -> (d : dirs, vids, imgs, oths)
        LikelyVideoFile v -> (dirs, v : vids, imgs, oths)
        LikelyImageFile i -> (dirs, vids, i : imgs, oths)
        LikelyOther t -> (dirs, vids, imgs, t : oths)
    )
    ([], [], [], [])

-- Other Helpers

rootDirectoryPath :: RootDirectoryLocation -> DirectoryPath
rootDirectoryPath r = DirectoryPath r []
