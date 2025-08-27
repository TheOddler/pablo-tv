{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import DB
import Data.List (sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, getCurrentTime)
import Database.Persist.Sqlite (ConnectionPool, PersistStoreWrite (..))
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import Path
import SaferIO (FSRead)
import System.Directory (listDirectory)
import System.FilePath (dropTrailingPathSeparator, hasExtension)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (logDuration, withDuration)

getVideoDirPath :: (FSRead m) => m (Path Abs Dir)
getVideoDirPath = do
  -- home <- getHomeDir
  -- let videoDirName = $(mkRelDir "Videos")
  pure $ $(mkAbsDir "/run/user/1000/gvfs/smb-share:server=192.168.0.99,share=videos")

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

      -- Update files
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

    -- TODO Idea:
    -- Read all known data (commented out above), and do a `getFileStatus` for any new directories to get when they were last modified.
    -- Then we'd have the modified timestamp on the directory, rather than an added timestamp on files.
    -- We could then update the modified timestamp with a good guess (good enough for what we need), by checking if there were any new files added (again, we can read all files we already know about).
    -- That way we do a bit more work for completely new directories (usually there will only be a few, only when creating the DB for the first time will there be many),
    -- and we'll have better timestamps for when new files are added (and we only really care about that on a per-directory level).
    -- Though, one problem with adding this timestamp to directories is that we'd either have to update all parent directories as well, or when reading check if there are any child directories to get a fully accurate timestamp.
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

-- Some helpers

readInt :: String -> Maybe Int
readInt = readMaybe

tryRegex :: Path x y -> ([String] -> Maybe a) -> String -> Maybe a
tryRegex source resultParser regex =
  let res :: (String, String, String, [String])
      res = toFilePath source =~ regex
      (_, _, _, matches) = res
   in resultParser matches

expect1Int :: [String] -> Maybe Int
expect1Int = \case
  [a] ->
    readInt a
  _ -> Nothing

expect2Ints :: [String] -> Maybe (Int, Int)
expect2Ints = \case
  [a, b] ->
    (,) <$> readInt a <*> readInt b
  _ -> Nothing

expect3Ints :: [String] -> Maybe (Int, Int, Int)
expect3Ints = \case
  [a, b, c] ->
    (,,) <$> readInt a <*> readInt b <*> readInt c
  _ -> Nothing

seasonFromDir :: Path a Dir -> Maybe Int
seasonFromDir dir =
  tryRegex dir expect1Int "[Ss]eason ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eries ([0-9]+)"
    <|> tryRegex dir expect1Int "[Ss]eizoen ([0-9]+)"

seasonFromFiles :: [Path a File] -> Maybe Int
seasonFromFiles files =
  case mSeason of
    Just season -> Just season
    Nothing -> if looseEpisodesFound then Just 1 else Nothing
  where
    mSeason = NE.head <$> listToMaybe (sortWith NE.length $ NE.group (mapMaybe seasonFromFile files))
    looseEpisodesFound = any (isJust . snd . episodeInfoFromFile) files

    seasonFromFile :: Path a File -> Maybe Int
    seasonFromFile file =
      tryRegex file expect1Int "[Ss]eason ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eries ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]eizoen ([0-9]+)"
        <|> tryRegex file expect1Int "[Ss]([0-9]+)[Ee][0-9]+"
        <|> tryRegex file expect1Int "([0-9]+)[Xx][0-9]+"

episodeInfoFromFile :: Path a File -> (Maybe Int, Maybe (Either Int (Int, Int)))
episodeInfoFromFile file =
  let double :: Maybe (Int, Int, Int)
      double =
        tryRegex file expect3Ints "[Ss]([0-9]+)[Ee]([0-9]+)-[Ee]([0-9]+)"

      seasonAndEp :: Maybe (Int, Int)
      seasonAndEp =
        tryRegex file expect2Ints "[Ss]([0-9]+)[Ee]([0-9]+)"
          <|> tryRegex file expect2Ints "([0-9]+)[Xx]([0-9]+)"

      epOnly :: Maybe Int
      epOnly =
        tryRegex file expect1Int "[Ee]pisode ([0-9]+)"
          <|> tryRegex file expect1Int "[Aa]flevering ([0-9]+)"
   in case double of
        Just (s, a, b) -> (Just s, Just $ Right (a, b))
        Nothing -> case seasonAndEp of
          Just (s, a) -> (Just s, Just $ Left a)
          Nothing -> case epOnly of
            Just a -> (Nothing, Just $ Left a)
            Nothing -> (Nothing, Nothing)

yearRegex :: String
yearRegex = "((19|20)[0-9][0-9])"

yearFromDir :: Path a Dir -> Maybe Int
yearFromDir dir =
  -- Take fst because the regex parses doesn't support non-capturing groups
  -- so we capture two, but only use the first, and ignore the inner group
  fst <$> tryRegex (dirname dir) expect2Ints yearRegex

yearFromFiles :: [Path a File] -> Maybe Int
yearFromFiles files =
  firstJusts (try <$> files)
  where
    try file = fst <$> tryRegex file expect2Ints yearRegex

titleFromDir :: Path a Dir -> Text
titleFromDir folder = T.strip . fst $ T.breakOn "(" (niceDirNameT folder)

niceDirNameT :: Path a Dir -> Text
niceDirNameT = T.pack . dropTrailingPathSeparator . fromRelDir . dirname

niceFileNameT :: Path a File -> Text
niceFileNameT file =
  let withoutExt = (fst <$> splitExtension file) `orElse` file
      name = fromRelFile $ filename withoutExt
   in T.replace "." " " $ T.pack name

isVideoFile :: Path b File -> Bool
isVideoFile file =
  case fileExtension file of
    Just ext -> ext `elem` [".mp4", ".mkv", ".avi", ".webm"]
    Nothing -> False

-- | Sorts the dirs, taking into account numbers properly
smartDirSort :: [Path Rel Dir] -> [Path Rel Dir]
smartDirSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceDirNameT a)
        (T.toLower $ niceDirNameT b)

-- | Sorts the dirs, taking into account numbers properly
smartFileSort :: [Path Rel File] -> [Path Rel File]
smartFileSort = sortBy sorting
  where
    sorting a b =
      Natural.compare
        (T.toLower $ niceFileNameT a)
        (T.toLower $ niceFileNameT b)
