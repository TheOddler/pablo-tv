{-# LANGUAGE QuasiQuotes #-}

module Directory.Paths where

import Control.Monad (forM)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.List (intercalate, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Directory.Directories
import Directory.Files
import GHC.Data.Maybe (firstJusts)
import Orphanage ()
import System.FilePath (joinPath, takeExtension, (</>))
import Util.TextWithoutSeparator
import Yesod (MonadIO (..), PathMultiPiece (..))

-- Raw Web Path

newtype RawWebPath = RawWebPath {unRawWebPath :: [TextWithoutSeparator]}
  deriving (Eq, Show, Read, Semigroup)

instance ToJSON RawWebPath where
  toJSON = toJSON . unsplitSeparatedText . unRawWebPath

instance FromJSON RawWebPath where
  parseJSON = withText "RawWebPath" $ \t -> do
    let split = splitAtSeparator t
    pure $
      if split == [[twsQQ||]]
        -- We can't really differentiate between [] and [""] so we prefer []
        then RawWebPath []
        else RawWebPath split

instance PathMultiPiece RawWebPath where
  fromPathMultiPiece :: [T.Text] -> Maybe RawWebPath
  fromPathMultiPiece parts = RawWebPath <$> forM parts textWithoutSeparator
  toPathMultiPiece :: RawWebPath -> [T.Text]
  toPathMultiPiece = map unTextWithoutSeparator . unRawWebPath

rawWebPathFromRoot :: RootDirectoryLocation -> RawWebPath
rawWebPathFromRoot root =
  RawWebPath . splitAtSeparator $
    rootDirectoryLocationToText root

rawWebPathToRoot :: RootDirectories -> RawWebPath -> Maybe (RootDirectoryLocation, [TextWithoutSeparator])
rawWebPathToRoot roots raw = do
  let rootLocs = Map.keys roots
      stripRoot :: [TextWithoutSeparator] -> RootDirectoryLocation -> Maybe (RootDirectoryLocation, [TextWithoutSeparator])
      stripRoot ts r = case unRawWebPath (rawWebPathFromRoot r) `stripPrefix` ts of
        Nothing -> Nothing
        Just parts -> Just (r, parts)
  firstJusts $ stripRoot (unRawWebPath raw) <$> rootLocs

rawWebPathFromDirectoryNames :: [DirectoryName] -> RawWebPath
rawWebPathFromDirectoryNames = RawWebPath . map unDirectoryName

rawWebPathFromDirectory :: DirectoryPath -> RawWebPath
rawWebPathFromDirectory dirPath =
  rawWebPathFromRoot dirPath.directoryPathRoot
    <> rawWebPathFromDirectoryNames dirPath.directoryPathNames

rawWebPathToDirectory :: RootDirectories -> RawWebPath -> Maybe DirectoryPath
rawWebPathToDirectory roots raw = case rawWebPathToVideoFileOrDirectory roots raw of
  Just (Left d) -> Just d
  _ -> Nothing

rawWebPathFromVideoFile :: VideoFilePath -> RawWebPath
rawWebPathFromVideoFile video =
  rawWebPathFromRoot video.videoFilePathRoot
    <> rawWebPathFromDirectoryNames video.videoFilePathNames
    <> RawWebPath [unVideoFileName video.videoFilePathName]

rawWebPathToVideoFile :: RootDirectories -> RawWebPath -> Maybe VideoFilePath
rawWebPathToVideoFile roots raw = case rawWebPathToVideoFileOrDirectory roots raw of
  Just (Right v) -> Just v
  _ -> Nothing

rawWebPathToVideoFileOrDirectory :: RootDirectories -> RawWebPath -> Maybe (Either DirectoryPath VideoFilePath)
rawWebPathToVideoFileOrDirectory roots raw =
  case rawWebPathToRoot roots raw of
    Nothing -> Nothing
    Just (root, []) -> Just $ Left $ DirectoryPath root []
    Just (root, n : ns) -> do
      let namesNE = n NE.:| ns
          potentialFileName = NE.last namesNE
          lastIsFile =
            hasVideoFileExt potentialFileName
              || hasImageFileExt potentialFileName
              || hasCommonFileExt potentialFileName
          dirNames = map DirectoryName $ NE.init namesNE
      Just $
        if lastIsFile
          then Right $ VideoFilePath root dirNames (VideoFileName potentialFileName)
          else Left $ DirectoryPath root $ map DirectoryName (n : ns)

rawWebPathToAbsPath :: (MonadIO m) => RootDirectories -> RawWebPath -> m (Maybe FilePath)
rawWebPathToAbsPath roots raw =
  case rawWebPathToRoot roots raw of
    Nothing -> pure Nothing
    Just (root, parts) -> do
      absRoot <- rootDirectoryLocationToAbsPath root
      let partsStr :: FilePath
          partsStr = joinPath $ map (T.unpack . unTextWithoutSeparator) parts
      pure $ Just $ absRoot </> partsStr

-- Directory paths

data DirectoryPath = DirectoryPath
  { directoryPathRoot :: RootDirectoryLocation,
    directoryPathNames :: [DirectoryName]
  }
  deriving (Show, Eq)

addSubDir :: DirectoryPath -> DirectoryName -> DirectoryPath
addSubDir (DirectoryPath root names) newName = DirectoryPath root $ names ++ [newName]

directoryPathToAbsPath :: (MonadIO m) => DirectoryPath -> m FilePath
directoryPathToAbsPath (DirectoryPath root dirNames) = do
  rootAbsPath <- rootDirectoryLocationToAbsPath root
  pure $ intercalate "/" $ rootAbsPath : (T.unpack . unwrap <$> dirNames)

-- Video File Paths

data VideoFilePath = VideoFilePath
  { videoFilePathRoot :: RootDirectoryLocation,
    videoFilePathNames :: [DirectoryName],
    videoFilePathName :: VideoFileName
  }
  deriving (Show, Eq)

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
