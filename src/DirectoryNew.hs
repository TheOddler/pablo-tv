{-# LANGUAGE RecordWildCards #-}

module DirectoryNew where

import DB
import Data.Time (NominalDiffTime, getCurrentTime)
import Database.Persist.Sqlite (ConnectionPool, PersistStoreWrite (..))
import Path
import System.Directory (listDirectory)
import System.FilePath (hasExtension)
import Util (logDuration, withDuration)

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

data StepInfo = StepInfo
  { stepDir :: Path Abs Dir,
    stepDuration :: NominalDiffTime,
    stepSubDirs :: [Path Abs Dir],
    stepSubFiles :: [Path Abs File],
    stepIgnoredPaths :: [FilePath]
  }

walkDir ::
  Path Abs Dir ->
  (StepInfo -> IO ()) ->
  IO ([Path Abs Dir], [Path Abs File], [FilePath])
walkDir rootDir onStep = do
  fst <$> step [rootDir] ([], [], [])
  where
    listDir' :: Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File], [FilePath])
    listDir' dir = do
      let absDirPath = fromAbsDir dir
      (relPaths, duration) <- withDuration $ listDirectory absDirPath
      let absPaths = map (absDirPath ++) relPaths
      let pathGuesses = map guessPathType absPaths
      let dirInfo@(subDirs, subFiles, ignoredPaths) = partitionPathTypes pathGuesses
      onStep
        StepInfo
          { stepDir = dir,
            stepDuration = duration,
            stepSubDirs = subDirs,
            stepSubFiles = subFiles,
            stepIgnoredPaths = ignoredPaths
          }
      pure dirInfo

    step ::
      [Path Abs Dir] ->
      ([Path Abs Dir], [Path Abs File], [FilePath]) ->
      IO (([Path Abs Dir], [Path Abs File], [FilePath]), [Path Abs Dir])
    step [] agg = pure (agg, [])
    step (dirTodo : nextDirs) agg = do
      dirInfo@(subDirs, _, _) <- listDir' dirTodo
      -- let newDirsToCheck = nextDirs <> subDirs -- Depth-first walk
      let newDirsToCheck = nextDirs <> subDirs -- Breadth-first walk
      let newAgg = agg <> dirInfo
      step newDirsToCheck newAgg

-- | Here we don't use the typed listDir as it's slow.
-- My hunch is that it actually does IO to check if something is a file or not, which is slow.
-- Instead, I use the untyped FilePath API here, and then make guesses on what is a file or dir.
updateData ::
  ConnectionPool ->
  Path Abs Dir ->
  IO ()
updateData dbConnPool root = logDuration "Updated data" $ do
  -- Read once from the DB, the rest we do in memory
  -- existingDirs' <-
  --   logDuration "Read all dirs" $
  --     runDBWithConn dbConnPool $
  --       selectList [] []
  now <- getCurrentTime

  -- We're likely to be using a network share for the data, so walking this dir is slow.
  -- That means, we have plenty of time on each step to write to the locally stored DB.
  -- It also means that the locally stored DB might be partially updated during a full update of the data. But that's fine, we're not doing brain surgery here, it's fine to have imperfect data at times, we just want it quickly.
  (allDirs, allFiles, ignoredPaths) <- walkDir root $ \StepInfo {..} -> do
    ((), dbUpdateDur) <- withDuration $ runDBWithConn dbConnPool $ do
      -- Make sure the directory exists
      _ <-
        repsert (DirectoryKey stepDir) (Directory stepDir)

      let fileUpdates :: [(Key VideoFile, VideoFile)]
          fileUpdates = flip map stepSubFiles $ \filePath ->
            ( VideoFileKey filePath,
              VideoFile
                { videoFilePath = filePath,
                  videoFileAdded = now,
                  videoFileWatched = Nothing
                }
            )
      repsertMany fileUpdates
    putStrLn $
      concat
        [ "Walked ",
          show stepDir,
          " (",
          show stepDuration,
          ") and updated (",
          show dbUpdateDur,
          ")"
        ]

  putStrLn $ "Found dirs: " ++ show allDirs
  putStrLn $ "Found files: " ++ show allFiles
  putStrLn $ "Ignored paths: " ++ show ignoredPaths

  pure ()
