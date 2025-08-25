module DirectoryNew where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import DB
import Data.HashMap.Strict qualified as HM
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (ConnectionPool)
import GHC.Utils.Misc (sortWith)
import Path
import SafeMaths (epochToUTCTime)
import System.Directory (listDirectory)
import System.FilePath (hasExtension)
import System.Posix
  ( FileStatus,
    accessTime,
    fileID,
    getFileStatus,
    isRegularFile,
    modificationTime,
  )
import Util (logDuration, withDuration, withKeyFromValue)
import Yesod (PersistQueryWrite (..), selectList, updateWhere, upsert, (=.), (==.))

-- | Reading the file info of a path is rather slow, so we want to minimise the ones we read it for.
-- So instead of reading it for every path, we first do a guess what the path is, and read it for dirs only.
-- There's also some paths we ignore, regardless of wether they are files or dirs.
data LikelyPathType
  = LikelyDir (Path Abs Dir)
  | LikelyFile (Path Abs File)
  | LikelyIgnored FilePath

-- | If this guess is wrong, we won't explore the directory.
-- This expects an absolute path
guessPathType :: FilePath -> LikelyPathType
guessPathType p = case p of
  "" -> LikelyIgnored p
  '.' : _ -> LikelyIgnored p
  _ | hasExtension p -> case parseAbsFile p of
    Just absPath -> LikelyFile absPath
    Nothing -> LikelyIgnored p
  _ -> case parseAbsDir p of
    Just absPath -> LikelyDir absPath
    Nothing -> LikelyIgnored p

partitionPathTypes :: [LikelyPathType] -> ([Path Abs Dir], [Path Abs File], [FilePath])
partitionPathTypes =
  foldr
    ( \guess (ds, fs, is) -> case guess of
        LikelyDir d -> (d : ds, fs, is)
        LikelyFile f -> (ds, f : fs, is)
        LikelyIgnored i -> (ds, fs, i : is)
    )
    ([], [], [])

data DirAction
  = DirShouldUpdate FileStatus
  | DirUpToDate
  | DirTurnedOutToBeFile (Path Abs File)

checkDir :: HM.HashMap (Path Abs Dir) Directory -> Path Abs Dir -> IO DirAction
checkDir existingDirsData path = do
  let rawPath = fromAbsDir path
  ((action, message), duration) <-
    withDuration $ do
      status <-
        getFileStatus $
          fromAbsDir path
      -- Check if this is actually really a directory, since we have the info now anyway
      if isRegularFile status
        then do
          case parseAbsFile rawPath of
            Nothing -> fail $ "Somehow the file status says file, but the path can't be parsed into a file path: " ++ rawPath
            Just p -> pure (DirTurnedOutToBeFile p, "Wrong dir guess, turned out to be a file: ")
        else do
          let modTime = epochToUTCTime $ modificationTime status
          let existingDir = HM.lookup path existingDirsData
          let checkTime = maybe (posixSecondsToUTCTime 0) directoryLastChecked existingDir
          if modTime >= checkTime
            then pure (DirShouldUpdate status, "Updating ")
            else
              pure
                ( DirUpToDate,
                  "Skipping (modified " ++ show modTime ++ ", checked " ++ show checkTime ++ ") "
                )

  putStrLn $
    concat
      [ message,
        show path,
        " (",
        show duration,
        ")"
      ]

  pure action

data DBUpdate
  = SetCheckedDir (Path Abs Dir)
  | UpsertDir (Path Abs Dir) FileStatus
  | UpsertFile (Path Abs File)
  | SetFilesForDir (Path Abs Dir) [Path Abs File]
  | DeleteDir (Path Abs Dir) -- Also deletes sub-files

-- | This gives the DBUpdates some order, as I want upserts to happen first
dbUpdateOrder :: DBUpdate -> Int
dbUpdateOrder = \case
  SetCheckedDir _ -> 0
  UpsertDir _ _ -> 1
  UpsertFile _ -> 2
  SetFilesForDir _ _ -> 3
  DeleteDir _ -> 4

-- | Here we don't use the typed listDir as it's slow.
-- My hunch is that it actually does IO to check if something is a file or not, which is slow.
-- Instead, I use the untyped FilePath API here, and then make guesses on what is a file or dir.
updateData ::
  ConnectionPool ->
  Path Abs Dir ->
  IO ()
updateData dbConnPool root = do
  -- Read once from the DB, the rest we do in memory
  existingDirs' <-
    logDuration "Read all dirs" $
      runDBWithConn dbConnPool $
        selectList [] []

  let existingDirs = HM.fromList $ map (withKeyFromValue directoryPath) existingDirs'
  updates <- loop existingDirs [root] []
  -- When done, update the DB with our findings
  -- Since we don't do this in a transaction this means it could be wrong, but well, so be it
  -- We don't want to bloc
  now <- liftIO getCurrentTime
  logDuration "Doing data updates" $
    runDBWithConn dbConnPool $ do
      forM_ (sortWith dbUpdateOrder updates) $ \case
        SetCheckedDir path ->
          updateWhere [DirectoryPath ==. path] [DirectoryLastChecked =. now]
        UpsertDir path status ->
          void $
            upsert
              Directory
                { directoryPath = path,
                  directoryLastChecked = now,
                  directoryFileID = fileID status,
                  directoryModificationTime = modificationTime status,
                  directoryAccessTime = accessTime status
                }
              [ DirectoryLastChecked =. now,
                DirectoryFileID =. fileID status,
                DirectoryModificationTime =. modificationTime status,
                DirectoryAccessTime =. accessTime status
              ]
        UpsertFile path ->
          void $
            upsert
              VideoFile
                { videoFilePath = path,
                  videoFileWatched = Nothing
                }
              []
        SetFilesForDir path files ->
          forM_ files $ \file ->
            upsert
              VideoFile
                { videoFilePath = file,
                  videoFileWatched = Nothing
                }
              []
        DeleteDir path ->
          deleteWhere [DirectoryPath ==. path]

  pure ()
  where
    loop ::
      HM.HashMap (Path Abs Dir) Directory ->
      [Path Abs Dir] ->
      [DBUpdate] ->
      IO [DBUpdate]
    loop _ [] agg = pure agg
    loop existingDirs (dirToCheck : restToCheck) dbUpdateAgg = do
      dirAction <- liftIO $ checkDir existingDirs dirToCheck
      case dirAction of
        DirUpToDate ->
          -- This dir is already done, so nothing more to do, just check the rests
          loop existingDirs restToCheck $ SetCheckedDir dirToCheck : dbUpdateAgg
        DirTurnedOutToBeFile filePath ->
          loop existingDirs restToCheck $
            dbUpdateAgg ++ [DeleteDir dirToCheck, UpsertFile filePath]
        DirShouldUpdate status -> do
          let dirPathRaw = fromAbsDir dirToCheck
          relPaths <-
            logDuration ("Reading dir for update " ++ show dirToCheck) . liftIO $
              -- Don't use the Path API for listDirectory because it does IO to figure out what paths are files and directories, and thus us slow (especially on network drives).
              -- Instead use the FilePath API, which is much faster.
              -- We then use guesses to minimise IO done beyond this.
              listDirectory dirPathRaw
          let absPaths = map (dirPathRaw ++) relPaths
          let pathGuesses = map guessPathType absPaths
          let (dirGuesses, fileGuesses, _ignored) = partitionPathTypes pathGuesses

          let newUpdates =
                [ SetFilesForDir dirToCheck fileGuesses,
                  UpsertDir dirToCheck status
                ]

          loop existingDirs (restToCheck ++ dirGuesses) (dbUpdateAgg ++ newUpdates)
