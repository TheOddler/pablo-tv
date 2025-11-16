{-# LANGUAGE DerivingStrategies #-}

module Directory where

import Control.Monad (forM, forM_)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.ByteString qualified as BS
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
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Utils.Exception (displayException)
import ImageScraper (ImageSearchFailure (..), tryFindImage)
import Logging (LogLevel (..), Logger, putLog)
import Orphanage ()
import PVar (PVar, modifyPVar, readPVar)
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getModificationTime,
    getXdgDirectory,
    renameFile,
  )
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO, catchAny)
import Util
  ( failE,
    logDuration,
    unsnocNE,
  )
import Util.TextWithoutSeparator
import Yesod (MonadIO (..))

type RootDirectories = Map.Map RootDirectoryLocation RootDirectoryData

getDirectoryAtPath :: RootDirectories -> DirectoryPath -> Maybe DirectoryData
getDirectoryAtPath roots (DirectoryPath wantedRoot []) = do
  root <- Map.lookup wantedRoot roots
  pure $ rootDirectoryAsDirectory root
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

loadRootsFromDisk :: (Logger m, MonadUnliftIO m) => m (Maybe RootDirectories)
loadRootsFromDisk = logDuration "Loaded roots from disk" $ do
  memoryDir <- getMemoryFileDir
  let memoryFile = memoryDir </> memoryFileName
  let safeDecode =
        eitherDecodeFileStrict memoryFile
          `catchAny` \e -> pure $ Left $ displayException e
  rootsOrErr <- liftIO safeDecode
  case rootsOrErr of
    Right roots -> pure $ Just roots
    Left err -> do
      putLog Error $ "Failed loading roots from disk, marking as broken. Error was: " ++ err
      liftIO (renameFile memoryFile (memoryFile ++ ".broken"))
        `catchAny` \e -> putLog Error $ "Failed marking as broken: " ++ displayException e
      pure Nothing

data ShallowDirUpdateResult
  = ShallowDirUpdateDirUnknown -- Nothing found at the given path, so nothing checked nor updated
  | ShallowDirUpdateDirNotFoundOnDisk
  | ShallowDirUpdateWasNotADir -- We found it, but turned out not to be a directory, we guessed wrong somewhere
  | ShallowDirUpdateUnchanged
  | ShallowDirUpdateChanged DirectoryData

-- | Does a shallow update, so updates only the directory itself, no sub-directories.
-- If it found new folders, it'll add them as empty folder, and do no further updates to them.
-- Can still throw if some some error happens we can't recover from and that probably indicated a bug or something I should look at at least.
shallowUpdateDirectory :: forall m. (MonadIO m, Logger m, MonadThrow m) => RootDirectories -> DirectoryPath -> m ShallowDirUpdateResult
shallowUpdateDirectory roots dirPath = do
  absDirPath <- directoryPathToAbsPath dirPath
  logDuration ("Shallow updated dir " ++ absDirPath) $ do
    let mCurrentDirData = getDirectoryAtPath roots dirPath
    case mCurrentDirData of
      Nothing -> pure ShallowDirUpdateDirUnknown
      Just currentDirData -> do
        namesOrErr <- safeCleanListDirectory absDirPath
        case namesOrErr of
          Left err | err.ioe_type == InappropriateType -> do
            pure ShallowDirUpdateWasNotADir
          Left err | err.ioe_type == NoSuchThing && null dirPath.directoryPathNames -> do
            pure ShallowDirUpdateDirNotFoundOnDisk
          Left err -> do
            failE ("Updating directory " ++ absDirPath) err
          Right names -> do
            let typeGuesses = guessType <$> names
            let (subDirGuesses, videoGuesses, imageGuesses, _ignored) = partitionPathTypes typeGuesses
            updatedSubDirs <- updateSubDirs subDirGuesses
            updatedVideoFiles <- updateVideoFiles videoGuesses
            updatedImageFile <- updateImageFile imageGuesses

            case (updatedSubDirs, updatedVideoFiles, updatedImageFile) of
              (Nothing, Nothing, Nothing) -> pure ShallowDirUpdateUnchanged
              _ -> do
                pure . ShallowDirUpdateChanged $
                  DirectoryData
                    { directoryImage = updatedImageFile,
                      directoryVideoFiles =
                        updatedVideoFiles `orElse` currentDirData.directoryVideoFiles,
                      directorySubDirs =
                        updatedSubDirs `orElse` currentDirData.directorySubDirs
                    }
            where
              updateSubDirs :: [DirectoryName] -> m (Maybe (Map.Map DirectoryName DirectoryData))
              updateSubDirs subDirGuesses = do
                let knownSubDirs = Map.keys currentDirData.directorySubDirs
                let newDirs = subDirGuesses \\ knownSubDirs
                let noLongerExist = knownSubDirs \\ subDirGuesses
                pure $ case (newDirs, noLongerExist) of
                  ([], []) -> Nothing -- Nothing indicates no changes were found
                  _ -> do
                    -- Step 1: Remove dirs that that no longer exist
                    let step1 = foldr Map.delete currentDirData.directorySubDirs noLongerExist
                    -- Step 2: Add new dirs as empty (they'll need to be separately shallow updated)
                    let emptyDir = DirectoryData Nothing Map.empty Map.empty
                    let step2 = foldr (`Map.insert` emptyDir) step1 newDirs
                    Just step2

              -- Checks if there are any new files or remove files.
              -- Files we already knew about are left alone to save some IO.
              updateVideoFiles :: [VideoFileName] -> m (Maybe (Map.Map VideoFileName VideoFileData))
              updateVideoFiles videoGuesses = do
                let knownVideoNames = Map.keys currentDirData.directoryVideoFiles
                let newVideoNames = videoGuesses \\ knownVideoNames
                let removedVideoNames = knownVideoNames \\ videoGuesses
                case (newVideoNames, removedVideoNames) of
                  ([], []) -> pure Nothing
                  _ -> do
                    -- Step 1: Remove files that no longer exist
                    let step1 = foldr Map.delete currentDirData.directoryVideoFiles removedVideoNames
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

              -- Updates the image if it's different from what we know about.
              -- If there's none on disk, and no known one either, it'll try to find one online.
              updateImageFile :: [ImageFileName] -> m (Maybe (ImageFileName, ImageFileData))
              updateImageFile imageGuesses = do
                -- TODO: Check if an image exists, before doing the sub dirs, and then pass on wether or not an image was found already, so child knows not to get another image for new folders for example
                -- We only care about the best image, other images ignore, even if there are new ones that aren't the best
                let diskImageName = bestImageFile imageGuesses
                    currentImageName = fst <$> currentDirData.directoryImage
                    anyParentDirHasImage = isJust $ findDirWithImageFor roots dirPath
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

                case (currentImageName, diskImageName) of
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

-- | Recursively updates the directory, each time a directory is updated it's saved to the root directories pvar.
-- It also saves all the directories data to disk at the end.
recursiveUpdateDirectory :: forall m. (MonadIO m, MonadThrow m, Logger m) => PVar RootDirectories -> DirectoryPath -> m ()
recursiveUpdateDirectory rootsPVar startingDirPath = do
  -- Update
  recursiveUpdateDirectoryNoSave rootsPVar startingDirPath
  -- Save the new result (saved in the PVar already) to disk
  newRoots <- readPVar rootsPVar
  saveRootsToDisk newRoots

-- `recursiveUpdateDirectory` but doesn't save to disk at the end.
recursiveUpdateDirectoryNoSave :: forall m. (MonadIO m, MonadThrow m, Logger m) => PVar RootDirectories -> DirectoryPath -> m ()
recursiveUpdateDirectoryNoSave rootsPVar startingDirPath = do
  absStartingDirPath <- directoryPathToAbsPath startingDirPath
  logDuration ("Fully updated dir " ++ absStartingDirPath) $
    loop [startingDirPath]
  where
    loop :: [DirectoryPath] -> m ()
    loop [] = pure ()
    loop (dirPath : rest) = do
      updatedRoots <- modifyPVar rootsPVar $ \roots -> do
        shallowUpdRes <- shallowUpdateDirectory roots dirPath
        absDirPath <- directoryPathToAbsPath dirPath
        case shallowUpdRes of
          ShallowDirUpdateDirUnknown -> do
            putLog Warning $ "Tried updating a directory we don't know about: " ++ absDirPath
            pure roots
          ShallowDirUpdateDirNotFoundOnDisk -> do
            putLog Warning $ "Tried updating a directory that doesn't exist on disk: " ++ absDirPath
            -- We might want to delete this dir, as it doesn't exist any more. BUT!
            -- It might also just not exist because the network disk it's a child of is temporarily unavailable.
            -- So instead, we don't do anything, and instead the user will have to either:
            -- 1. Manually update the parent of this directory
            -- 2. Manually delete the whole root directory this is part of
            -- TODO: Is this too cautious? Should we delete it anyway? Or check if the root exists, and only delete if not?
            pure roots
          ShallowDirUpdateWasNotADir -> do
            -- Something existed on the path, but it wasn't a directory. We probably guessed wrong, so (potentially) remove it:
            case NE.nonEmpty dirPath.directoryPathNames of
              Nothing ->
                -- This is a root directory, we never want to remove those
                pure roots
              Just namesNE -> do
                let (inits, name) = unsnocNE namesNE
                let parentDir = DirectoryPath dirPath.directoryPathRoot inits
                pure $ updateDirectoryAtPath roots parentDir $ \dirData ->
                  dirData
                    { directorySubDirs = Map.delete name dirData.directorySubDirs
                    }
          ShallowDirUpdateUnchanged ->
            pure roots
          ShallowDirUpdateChanged updatedDirData ->
            pure $ updateDirectoryAtPath roots dirPath $ const updatedDirData

      -- Add sub directories to the list of stuff to update in our loop
      let newDirData = getDirectoryAtPath updatedRoots dirPath
          nextDirNames = maybe [] (Map.keys . directorySubDirs) newDirData
          nextDirPaths = addSubDir dirPath <$> nextDirNames
      loop $ rest ++ nextDirPaths

recursivelyUpdateAllDirectories :: (MonadIO m, MonadThrow m, Logger m) => PVar RootDirectories -> m ()
recursivelyUpdateAllDirectories rootsPVar = do
  -- Do all updates without saving
  logDuration "Updated all directories" $ do
    roots <- readPVar rootsPVar
    forM_ (Map.keys roots) $ \rootLocation -> do
      recursiveUpdateDirectoryNoSave rootsPVar $ rootDirectoryPath rootLocation
  -- At the end save to disk
  newRoots <- readPVar rootsPVar
  saveRootsToDisk newRoots

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
