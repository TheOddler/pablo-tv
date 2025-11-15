{-# LANGUAGE DerivingStrategies #-}

module Directory where

import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Foldable (foldrM)
import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Directory.Directories
import Directory.Files
import Directory.Paths
import GHC.Data.Maybe (orElse)
import GHC.IO (catchAny)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Utils.Exception (displayException)
import ImageScraper (ImageSearchFailure (..), tryFindImage)
import Logging (LogLevel (..), Logger, putLog)
import Orphanage ()
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getModificationTime,
    getXdgDirectory,
  )
import System.FilePath ((</>))
import Util
  ( failE,
    impossible,
    logDuration,
  )
import Util.TextWithoutSeparator
import Yesod (MonadIO (..))

type RootDirectories = Map.Map RootDirectoryLocation RootDirectoryData

getDirectoryAtPath :: RootDirectories -> DirectoryPath -> Maybe DirectoryData
getDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) = do
  root <- Map.lookup wantedRoot roots
  innerGet wantedDirNames $ rootDirectoryAsDirectory root
  where
    innerGet :: [DirectoryName] -> DirectoryData -> Maybe DirectoryData
    innerGet [] _ = Nothing
    innerGet [name] dir =
      Map.lookup name dir.directorySubDirs
    innerGet (name : rest) dir = do
      subDir <- Map.lookup name dir.directorySubDirs
      innerGet rest subDir

-- | Looks for an image in the directory or any of it's parents, preferring closer to the directory
findDirWithImageFor :: RootDirectories -> DirectoryPath -> Maybe DirectoryPath
findDirWithImageFor allRoots (DirectoryPath dirRoot dirNames) = innerSearch (NE.nonEmpty dirNames)
  where
    innerSearch Nothing = Nothing
    innerSearch (Just dirNamesNE) = do
      let dirPath = DirectoryPath dirRoot $ NE.toList dirNamesNE
      case getDirectoryAtPath allRoots dirPath of
        Nothing -> Nothing
        Just d ->
          case d.directoryImage of
            Nothing -> innerSearch (NE.nonEmpty $ NE.init dirNamesNE)
            Just _img -> Just dirPath

-- | Will silently do nothing if the path isn't found.
updateDirectoryAtPath :: RootDirectories -> DirectoryPath -> (DirectoryData -> DirectoryData) -> RootDirectories
updateDirectoryAtPath roots (DirectoryPath wantedRoot wantedDirNames) updateFunc =
  Map.adjust
    ( let dirBackToRoot dir =
            RootDirectoryData
              { rootDirectorySubDirs = dir.directorySubDirs,
                rootDirectoryVideoFiles = dir.directoryVideoFiles
              }
       in dirBackToRoot . innerUpdate wantedDirNames . rootDirectoryAsDirectory
    )
    wantedRoot
    roots
  where
    innerUpdate :: [DirectoryName] -> DirectoryData -> DirectoryData
    innerUpdate [] dirData = updateFunc dirData
    innerUpdate (name : ns) dirData =
      dirData
        { directorySubDirs = Map.adjust (innerUpdate ns) name dirData.directorySubDirs
        }

data DirectoryUpdateResult
  = DirectoryUnchanged
  | DirectoryChanged DirectoryData
  | DirectoryNotADirectory
  | -- | A special case, it's possible for a root not to be found, if for example the samba share isn't mounted.
    DirectoryNotFoundRoot

updateRootDirectoriesFromDisk :: (MonadIO m, MonadThrow m, Logger m) => RootDirectories -> m RootDirectories
updateRootDirectoriesFromDisk rootDirs =
  flip Map.traverseWithKey rootDirs $ \rootLocation rootDirData -> do
    let path = rootDirectoryPath rootLocation
    let rootAsDir = rootDirectoryAsDirectory rootDirData
    result <- updateDirectoryFromDisk rootDirs path rootAsDir
    case result of
      DirectoryChanged updated ->
        pure
          RootDirectoryData
            { rootDirectorySubDirs = updated.directorySubDirs,
              rootDirectoryVideoFiles = updated.directoryVideoFiles
            }
      DirectoryUnchanged -> pure rootDirData
      DirectoryNotADirectory -> do
        putLog Warning $
          unwords
            [ "Root directory",
              T.unpack $ unwrap $ unRootDirectoryLocation rootLocation,
              "wasn't a directory somehow?"
            ]
        pure rootDirData
      DirectoryNotFoundRoot ->
        -- Don't remove the root dir, this should only be done manually.
        pure rootDirData

memoryFileName :: FilePath
memoryFileName = "pablo-tv.json"

getMemoryFileDir :: (MonadIO m) => m FilePath
getMemoryFileDir = liftIO $ getXdgDirectory XdgData ""

-- | Writes the known disks to a json file on disk, so we can read it on startup next time.
saveRootsToDisk :: (MonadIO m, Logger m) => RootDirectories -> m ()
saveRootsToDisk roots = logDuration "Saved roots to disk" $ do
  memoryDir <- getMemoryFileDir
  liftIO $ do
    createDirectoryIfMissing True memoryDir
    encodeFile (memoryDir </> memoryFileName) roots

loadRootsFromDisk :: (Logger m, MonadIO m) => m (Maybe RootDirectories)
loadRootsFromDisk = do
  memoryDir <- getMemoryFileDir
  let memoryFile = memoryDir </> memoryFileName
  let safeDecode =
        eitherDecodeFileStrict memoryFile
          `catchAny` \e -> pure $ Left $ displayException e
  rootsOrErr <- logDuration "Loaded roots from disk" $ liftIO safeDecode
  case rootsOrErr of
    Right roots -> pure $ Just roots
    Left err -> do
      putLog Error $ "Failed loading roots from disk: " ++ err
      pure Nothing

-- | Updates the directory (at given path) with new data from disk.
-- It will potentially remove or add video files and sub-directories.
-- It leaves watched or added information for files in tact.
updateDirectoryFromDisk :: forall m. (MonadIO m, MonadThrow m, Logger m) => RootDirectories -> DirectoryPath -> DirectoryData -> m DirectoryUpdateResult
updateDirectoryFromDisk allDirsData dirPath dir = do
  absDirPath <- directoryPathToAbsPath dirPath
  logDuration ("Updated directory " ++ absDirPath) $ do
    namesOrErr <- safeCleanListDirectory absDirPath
    case namesOrErr of
      Left err | err.ioe_type == InappropriateType -> do
        putLog Warning $ "What we thought was a directory turned out not to be: " ++ absDirPath
        pure DirectoryNotADirectory
      Left err | err.ioe_type == NoSuchThing && null dirPath.directoryPathNames -> do
        putLog Warning $ "Root folder not found: " ++ absDirPath
        pure DirectoryNotFoundRoot
      Left err -> do
        failE ("Updating directory " ++ absDirPath) err
      Right names -> do
        let typeGuesses = guessType <$> names
        let (subDirGuesses, videoGuesses, imageGuesses, _ignored) = partitionPathTypes typeGuesses
        -- Recursively check for directory updates
        subDirUpdates <- forM subDirGuesses $ \subDirName -> do
          let knownDir = Map.lookup subDirName dir.directorySubDirs
          let newDir = DirectoryData Nothing Map.empty Map.empty
          let fullPath = addSubDir dirPath subDirName
          upd <- updateDirectoryFromDisk allDirsData fullPath $ knownDir `orElse` newDir
          pure (subDirName, upd)
        (unchanged, changed, notDirs) <-
          foldrM
            ( \(name, upd) (un, ch, nd) -> case upd of
                DirectoryUnchanged -> pure (name : un, ch, nd)
                DirectoryChanged d -> pure (un, (name, d) : ch, nd)
                DirectoryNotADirectory -> pure (un, ch, name : nd)
                DirectoryNotFoundRoot -> impossible "This should only happen on roots, and thus should be impossible here."
            )
            ([], [], [])
            subDirUpdates
        let actualSubDirs = subDirGuesses \\ notDirs
        let noLongerExist = Map.keys dir.directorySubDirs \\ actualSubDirs
        let updatedSubDirs :: Maybe (Map.Map DirectoryName DirectoryData)
            updatedSubDirs = case (unchanged, changed, noLongerExist) of
              (_, [], []) -> Nothing -- Nothing indicates no changes were found
              _ -> do
                -- Step 1: Remove dirs that that no longer exist
                let step1 = foldr Map.delete dir.directorySubDirs noLongerExist
                -- Step 2: Update the updated dirs (insert overwrites)
                let step2 = foldr (uncurry Map.insert) step1 changed
                Just step2

        -- Check if there are any new files or remove files
        -- Files we already knew about, we won't check again so we don't do unneeded IO
        let knownVideoNames = Map.keys dir.directoryVideoFiles
        let newVideoNames = videoGuesses \\ knownVideoNames
        let removedVideoNames = knownVideoNames \\ videoGuesses
        updatedVideoFiles <- case (newVideoNames, removedVideoNames) of
          ([], []) -> pure Nothing
          _ -> do
            -- Step 1: Remove files that no longer exist
            let step1 = foldr Map.delete dir.directoryVideoFiles removedVideoNames
            -- Step 2: Add new videos
            newVideos <- forM newVideoNames $ \newVideoName -> do
              let fullFilePath = absDirPath ++ "/" ++ T.unpack (unwrap newVideoName.unVideoFileName)
              modTime <- liftIO $ getModificationTime fullFilePath
              pure
                ( newVideoName,
                  VideoFileData
                    { videoFileAdded = modTime,
                      videoFileWatched = Nothing
                    }
                )
            let step2 = foldr (uncurry Map.insert) step1 newVideos
            pure $ Just step2

        -- We only care about the best image, other images ignore, even if there are new ones that aren't the best
        let diskImageName = bestImageFile imageGuesses
            currentImageName = fst <$> dir.directoryImage
            anyParentDirHasImage = isJust $ findDirWithImageFor allDirsData dirPath
            isRootDir = null dirPath.directoryPathNames
            isSeasonDirOrSubDir = any isSeasonDir dirPath.directoryPathNames

            getImageFromDisk :: ImageFileName -> m (ImageFileName, ImageFileData)
            getImageFromDisk imgName = do
              putLog Info $ "Getting image from disk for: " ++ absDirPath
              imgFile <- liftIO $ BS.readFile $ absDirPath </> T.unpack (unwrap imgName)
              pure (imgName, ImageFileData imgFile)

            imgFindingErr msg = "Failed finding image for " ++ absDirPath ++ ": " ++ msg
            tryFindImage' searchTerm = do
              mImg <- tryFindImage searchTerm
              case mImg of
                Left ImageSearchFailedScraping -> do
                  putLog Warning $ imgFindingErr "web scraping unsuccessful"
                  pure Nothing
                Left (ImageSearchDownloadFailed err) -> do
                  putLog Warning $ imgFindingErr err
                  pure Nothing
                Right (contentType, img) -> do
                  putLog Info $ "Downloaded image for " ++ absDirPath ++ " with search term " ++ T.unpack searchTerm
                  pure $ Just (imageFileNameForContentType contentType, ImageFileData img)

            dirName =
              NE.last $
                unRootDirectoryLocation dirPath.directoryPathRoot
                  NE.:| map unDirectoryName dirPath.directoryPathNames
            getImageFromWeb = do
              firstAttempt <- tryFindImage' $ unTextWithoutSeparator dirName
              case firstAttempt of
                Just img -> pure $ Just img
                Nothing -> tryFindImage' . fst $ splitTitleFromDir $ DirectoryName dirName
        newImage <- case (currentImageName, diskImageName) of
          (Nothing, Just diskImg) ->
            -- We currently don't know about an image, but there's one on disk, use that.
            Just <$> getImageFromDisk diskImg
          (Just img, Just diskImg)
            | img /= diskImg ->
                -- The image we know about is different from the one on disk, update it!
                Just <$> getImageFromDisk diskImg
          (Just _img, Just _diskImg) ->
            -- The image we know about is the same as on disk, so no need to update, save some IO
            pure Nothing
          (Just _img, Nothing) ->
            -- We have an image already, but none on disk. Just keep it, we probably downloaded it previously.
            pure Nothing
          (Nothing, Nothing)
            | isRootDir -- Root dirs never have images
                || isSeasonDirOrSubDir -- Season dirs or subdirs of season don't have images, otherwise we search for "Season X" a bunch. Subdirs are likely stuff like "subs"
                || anyParentDirHasImage -> -- If the parent dir already has an image we use that image, so save some time and don't search for another image
                pure Nothing
          (Nothing, Nothing) ->
            -- Nothing known, nothing on disk, try and download one
            getImageFromWeb

        case (updatedSubDirs, updatedVideoFiles, newImage) of
          (Nothing, Nothing, Nothing) -> pure DirectoryUnchanged
          _ -> do
            pure . DirectoryChanged $
              DirectoryData
                { directoryImage = newImage,
                  directoryVideoFiles =
                    updatedVideoFiles `orElse` dir.directoryVideoFiles,
                  directorySubDirs =
                    updatedSubDirs `orElse` dir.directorySubDirs
                }

-- Agg data
data AggDirInfo = AggDirInfo
  { aggDirName :: DirectoryName,
    aggDirPath :: DirectoryPath,
    aggDirLastModified :: UTCTime,
    aggDirLastWatched :: UTCTime,
    aggDirVideoFileCount :: Int,
    aggDirPlayedVideoFileCount :: Int
  }

foldFilesDataRecur :: forall a. (VideoFileData -> a -> a) -> a -> DirectoryData -> a
foldFilesDataRecur updateAgg agg dir = loop agg [dir]
  where
    loop :: a -> [DirectoryData] -> a
    loop a [] = a
    loop a (dirTodo : restDirs) = do
      let newA = foldr updateAgg a dirTodo.directoryVideoFiles
          subDirs = Map.elems dirTodo.directorySubDirs
      loop newA $ restDirs ++ subDirs

getSubDirAggInfo :: DirectoryPath -> DirectoryData -> [AggDirInfo]
getSubDirAggInfo dirPath dir = do
  let epoch = posixSecondsToUTCTime 0
  flip map (Map.toList dir.directorySubDirs) $ \(subDirName, subDirData) ->
    foldFilesDataRecur
      ( \videoFileData agg ->
          AggDirInfo
            agg.aggDirName
            agg.aggDirPath
            (max agg.aggDirLastModified videoFileData.videoFileAdded)
            ( case videoFileData.videoFileWatched of
                Nothing -> agg.aggDirLastWatched
                Just w -> max agg.aggDirLastWatched w
            )
            (agg.aggDirVideoFileCount + 1)
            ( case videoFileData.videoFileWatched of
                Nothing -> agg.aggDirPlayedVideoFileCount
                Just _ -> agg.aggDirPlayedVideoFileCount + 1
            )
      )
      ( AggDirInfo
          subDirName
          (addSubDir dirPath subDirName)
          epoch
          epoch
          0
          0
      )
      subDirData
