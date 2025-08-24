module DirectoryNew where

import Control.Monad (forM)
import Data.Time (NominalDiffTime)
import Path
import System.Directory (listDirectory)
import System.FilePath (hasExtension)
import Util (withDuration)

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

-- | Here we don't use the typed listDir as it's slow.
-- My hunch is that it actually does IO to check if something is a file or not, which is slow.
-- Instead, I use the untyped FilePath API here, and then make guesses on what is a file or dir.
walkDirDepthFirst ::
  (Path Abs Dir -> NominalDiffTime -> IO ()) ->
  Path Abs Dir ->
  IO ([Path Abs Dir], [Path Abs File], [FilePath])
walkDirDepthFirst onStep root = do
  let rootPath = fromAbsDir root
  (relPaths, duration) <- withDuration $ listDirectory rootPath
  onStep root duration
  let absPaths = map (rootPath ++) relPaths
  let pathGuesses = map guessPathType absPaths

  let (dirs, files, ignored) = partitionPathTypes pathGuesses

  (subDirs, subFiles, subIgnored) <- unzip3 <$> forM dirs (walkDirDepthFirst onStep)

  let allDirs = concat $ dirs : subDirs
  let allFiles = concat $ files : subFiles
  let allIgnored = concat $ ignored : subIgnored

  pure (allDirs, allFiles, allIgnored)

walkDirBreadthFirst ::
  (Path Abs Dir -> NominalDiffTime -> IO ()) ->
  Path Abs Dir ->
  IO ([Path Abs Dir], [Path Abs File], [FilePath])
walkDirBreadthFirst onStep rootDir = do
  fst <$> step [rootDir] ([], [], [])
  where
    listDir' :: Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File], [FilePath])
    listDir' dir = do
      let absDirPath = fromAbsDir dir
      (relPaths, duration) <- withDuration $ listDirectory absDirPath
      onStep dir duration
      let absPaths = map (absDirPath ++) relPaths
      let pathGuesses = map guessPathType absPaths
      pure $ partitionPathTypes pathGuesses

    step ::
      [Path Abs Dir] ->
      ([Path Abs Dir], [Path Abs File], [FilePath]) ->
      IO (([Path Abs Dir], [Path Abs File], [FilePath]), [Path Abs Dir])
    step [] agg = pure (agg, [])
    step (dirTodo : nextDirs) (aggDirs, aggFiles, aggIgnored) = do
      (subDirs, subFiles, subIgnored) <- listDir' dirTodo
      let newAgg = (aggDirs <> subDirs, aggFiles <> subFiles, aggIgnored <> subIgnored)
      let newDirsToCheck = nextDirs <> subDirs
      step newDirsToCheck newAgg
