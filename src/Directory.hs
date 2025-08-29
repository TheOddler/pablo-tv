{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import Control.Monad (forM_, void)
import DB
import Data.ByteString qualified as BS
import Data.HashSet qualified as Set
import Data.List (intercalate, sortBy)
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, getCurrentTime)
import Database.Persist.Sqlite (ConnectionPool, Entity (..), PersistStoreWrite (..), PersistUniqueWrite (..), upsert, (=.))
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Utils.Exception (displayException, tryIO)
import Logging (LogLevel (..), logDuration, putLog)
import Path
import SaferIO (FSRead (..))
import System.Directory (listDirectory)
import System.FilePath (dropTrailingPathSeparator, takeExtension, takeFileName)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (safeMinimumOn, withDuration)

getVideoDirPath :: (FSRead m) => m (Path Abs Dir)
getVideoDirPath = do
  -- home <- getHomeDir
  -- let videoDirName = $(mkRelDir "Videos")
  -- pure $ home </> videoDirName
  pure $ $(mkAbsDir "/run/user/1000/gvfs/smb-share:server=192.168.0.99,share=videos")

-- | Reading the file info of a path is rather slow, so we want to minimise the ones we read it for.
-- So instead of reading it for every path, we first do a guess what the path is, and read it for dirs only.
-- There's also some paths we ignore, regardless of wether they are files or dirs.
data LikelyPathType
  = LikelyDir (Path Abs Dir)
  | LikelyVideoFile (Path Abs File)
  | LikelyImageFile (Path Abs File)
  | LikelyIgnored FilePath

-- | If this guess is wrong, we won't explore the directory.
-- This expects just the file or directory name
guessPathType :: Path Abs Dir -> FilePath -> LikelyPathType
guessPathType basePath p = case p of
  "" -> LikelyIgnored p
  '.' : _ -> LikelyIgnored p
  _ -> case parseRelFile p of
    Just relPath -> case fileExtension relPath of
      Nothing -> tryDir
      Just ext | isVideoFileExt ext -> LikelyVideoFile $ basePath </> relPath
      Just ext | isImageFileExt ext -> LikelyImageFile $ basePath </> relPath
      Just ext | isCommonFileExt ext -> LikelyIgnored p
      Just _ext -> tryDir -- Probably just a directory with a . in it's name
    Nothing -> tryDir
  where
    tryDir = case parseRelDir p of
      Just relPath -> LikelyDir $ basePath </> relPath
      Nothing -> LikelyIgnored p

partitionPathTypes :: [LikelyPathType] -> ([Path Abs Dir], [Path Abs File], [Path Abs File], [FilePath])
partitionPathTypes =
  foldr
    ( \guess (dirs, vids, imgs, igns) -> case guess of
        LikelyDir d -> (d : dirs, vids, imgs, igns)
        LikelyVideoFile v -> (dirs, v : vids, imgs, igns)
        LikelyImageFile i -> (dirs, vids, i : imgs, igns)
        LikelyIgnored i -> (dirs, vids, imgs, i : igns)
    )
    ([], [], [], [])

data StepInfo = StepInfo
  { stepDir :: Path Abs Dir,
    stepDuration :: NominalDiffTime,
    stepVideoFiles :: [Path Abs File],
    stepImageFiles :: [Path Abs File]
  }

walkDir ::
  Path Abs Dir ->
  (StepInfo -> IO ()) ->
  IO ([Path Abs Dir], [Path Abs File], [Path Abs File], [FilePath])
walkDir rootDir onStep = do
  fst <$> step [rootDir] ([], [], [], [])
  where
    listDir' :: Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File], [Path Abs File], [FilePath])
    listDir' dir = do
      let absDirPath = fromAbsDir dir
      tryIO (withDuration $ listDirectory absDirPath) >>= \case
        Left err -> do
          case ioe_type err of
            InappropriateType ->
              -- This dir is actually a file. It didn't have the right extension, as we guessed wrong, so return it as an ignored path.
              -- TODO Should I do something more with this information?
              putLog Warning $ "Guessed directory but turns out to be a file: " ++ show dir
            _ ->
              putLog Error $ "Error while trying to list directory: " ++ displayException err
          pure ([], [], [], [fromAbsDir dir])
        Right (relPaths, duration) -> do
          let pathGuesses = map (guessPathType dir) relPaths
          let dirInfo = partitionPathTypes pathGuesses
          let (_dirs, videoFiles, imageFiles, _ignoredPaths) = dirInfo
          onStep
            StepInfo
              { stepDir = dir,
                stepDuration = duration,
                stepVideoFiles = videoFiles,
                stepImageFiles = imageFiles
              }
          pure dirInfo

    step ::
      [Path Abs Dir] ->
      ([Path Abs Dir], [Path Abs File], [Path Abs File], [FilePath]) ->
      IO (([Path Abs Dir], [Path Abs File], [Path Abs File], [FilePath]), [Path Abs Dir])
    step [] agg = pure (agg, [])
    step (dirTodo : nextDirs) agg = do
      dirInfo@(subDirs, _, _, _) <- listDir' dirTodo
      -- let newDirsToCheck = nextDirs <> subDirs -- Depth-first walk
      let newDirsToCheck = nextDirs <> subDirs -- Breadth-first walk
      let newAgg = agg <> dirInfo
      step newDirsToCheck newAgg

bestImageFile :: [Path Abs File] -> Maybe (Path Abs File)
bestImageFile = safeMinimumOn $ \path ->
  case lower . fromRelFile . fst <$> splitExtension (filename path) of
    Just "poster" -> 0 :: Int
    _ -> 100

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
  (allDirs, allVideoFiles, allImageFiles, ignoredPaths) <- walkDir root $ \StepInfo {..} -> do
    ((), dbUpdateDur) <- withDuration $ do
      -- Find best image
      let mBestImg = bestImageFile stepImageFiles
      -- Try and do as much in a single transaction, but we might need some followup work that reads from disk and then does a second transaction
      (followupAction :: IO ()) <- runDBWithConn dbConnPool $ do
        -- Add dir, and if it already exists leave the image as is (we might update that later in the followup action)
        existingDir <-
          upsert
            (Directory stepDir Nothing Nothing)
            $ (DirectoryPath =. stepDir)
              : case mBestImg of
                Nothing ->
                  -- When there is no image in the directory, we remove whatever is in the DB as the image is gone
                  [ DirectoryImageName =. Nothing,
                    DirectoryImage =. Nothing
                  ]
                Just _ ->
                  -- We'll set the image in the followup as it requires slow IO
                  []

        -- Insert/update video files
        forM_ stepVideoFiles $ \filePath ->
          insertUnique_
            VideoFile
              { videoFileParent = entityKey existingDir,
                videoFileName = filename filePath,
                videoFileAdded = now,
                videoFileWatched = Nothing
              }

        -- Should we delete files that are no longe there?

        -- Figure out if we should do another update after.
        -- We do this in two steps because this followup action can then do slow IO, like reading the image from disk
        let mExistingImgName = directoryImageName (entityVal existingDir)
        let followup :: IO ()
            followup = case (mBestImg, mExistingImgName) of
              (Just foundImg, Just existingImgName)
                | filename foundImg == existingImgName ->
                    -- The best image we found is already in the db, nothing more to do
                    -- TODO: Maybe we should just update it always? This would require reading every time, but then we can replace the poster with an image with the same name
                    pure ()
              (Just foundImg, _) -> do
                -- We found an image, but it's not the one in the DB yet (might be none in the db)
                -- So we update the directory in the db.
                -- Since this requires slow IO, we do this in the followup.
                -- It's an update, in case between this and the followup the directory got removed, it should then probably remain removed.
                imageData <- BS.readFile $ fromAbsFile foundImg
                void . runDBWithConn dbConnPool $
                  update
                    (entityKey existingDir)
                    [ DirectoryImageName =. Just (filename foundImg),
                      DirectoryImage =. Just imageData
                    ]
              (Nothing, _) ->
                -- If there were no images, we already removed it from the DB, so nothing to do in the followup
                pure ()
        pure followup

      -- Run this followup action outside the transaction so we don't block too long with slow IO
      followupAction

    -- TODO Idea:
    -- Read all known data (commented out above), and do a `getFileStatus` for any new directories to get when they were last modified.
    -- Then we'd have the modified timestamp on the directory, rather than an added timestamp on files.
    -- We could then update the modified timestamp with a good guess (good enough for what we need), by checking if there were any new files added (again, we can read all files we already know about).
    -- That way we do a bit more work for completely new directories (usually there will only be a few, only when creating the DB for the first time will there be many),
    -- and we'll have better timestamps for when new files are added (and we only really care about that on a per-directory level).
    -- Though, one problem with adding this timestamp to directories is that we'd either have to update all parent directories as well, or when reading check if there are any child directories to get a fully accurate timestamp.
    putLog Info $
      concat
        [ "Walked ",
          show stepDir,
          " (",
          show stepDuration,
          ") and updated (",
          show dbUpdateDur,
          ")"
        ]

  let safeDropPrefix :: Path Abs a -> String
      safeDropPrefix p = case stripProperPrefix root p of
        Nothing -> toFilePath p
        Just withoutPrefix -> toFilePath withoutPrefix
  putLog Info $ "Found dirs: " ++ show (map safeDropPrefix allDirs)
  putLog Info $ "Found video files: " ++ show (map safeDropPrefix allVideoFiles)
  putLog Info $ "Found image files: " ++ show (map safeDropPrefix allImageFiles)
  putLog Info $ "Ignored paths: " ++ show (map takeFileName ignoredPaths)

  let showUnique :: [String] -> String
      showUnique els = intercalate ", " $ Set.toList $ Set.fromList els
  putLog Info $
    "Found video files extensions: "
      ++ showUnique (mapMaybe fileExtension allVideoFiles)
  putLog Info $
    "Found image files extensions: "
      ++ showUnique (mapMaybe fileExtension allImageFiles)
  let isHiddenPath = \case
        '.' : _ -> True
        _ -> False
      ignoredNonHidden = filter (not . isHiddenPath) ignoredPaths
      ignoredHidden = filter isHiddenPath ignoredPaths
  putLog Info $
    "Ignored paths extensions: "
      ++ showUnique (map takeExtension ignoredNonHidden)
  putLog Info $
    "Ignored hidden paths extensions: "
      ++ showUnique (map takeExtension ignoredHidden)

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

isVideoFileExt :: String -> Bool
isVideoFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".avi",
        ".flv",
        ".m4v",
        ".mkv",
        ".mov",
        ".mp4",
        ".mpeg",
        ".mpg",
        ".webm",
        ".wmv"
      ]

isImageFileExt :: String -> Bool
isImageFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".bmp",
        ".gif",
        ".heic",
        ".heif",
        ".jpeg",
        ".jpg",
        ".png",
        ".svg",
        ".tif",
        ".tiff",
        ".webp"
      ]

-- | For non-video, non-image ext.
-- This is used to improve guesses on what are files and what are directories.
isCommonFileExt :: String -> Bool
isCommonFileExt ext =
  ext
    `Set.member` Set.fromList
      [ ".7z",
        ".aac",
        ".apk",
        ".bat",
        ".csv",
        ".db",
        ".dmg",
        ".doc",
        ".docx",
        ".exe",
        ".flac",
        ".gz",
        ".idx",
        ".img",
        ".iso",
        ".mp3",
        ".msi",
        ".odp",
        ".ods",
        ".odt",
        ".ogg",
        ".pdf",
        ".ppt",
        ".pptx",
        ".rar",
        ".rtf",
        ".sh",
        ".srt",
        ".sub",
        ".tar.gz",
        ".tar",
        ".txt",
        ".wav",
        ".xls",
        ".xlsx",
        ".yaml",
        ".zip"
      ]

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
