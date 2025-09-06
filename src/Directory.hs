{-# LANGUAGE RecordWildCards #-}

module Directory where

import Algorithms.NaturalSort qualified as Natural
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, readTQueue, writeTQueue)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Monad (forM_, forever, void)
import DB
import Data.ByteString qualified as BS
import Data.HashSet qualified as Set
import Data.List (intercalate, sortBy)
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite (ConnectionPool, Entity (..), PersistStoreWrite (..), PersistUniqueWrite (..), upsert, (=.))
import GHC.Data.Maybe (firstJusts, orElse)
import GHC.Exts (sortWith)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Utils.Exception (displayException, tryIO)
import Logging (LogLevel (..), putLog)
import Path
import System.Directory (listDirectory)
import System.FilePath (dropTrailingPathSeparator, takeExtension)
import TVDB (TVDBData (..), TVDBToken, TVDBType (..), getInfoFromTVDB)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (getImageContentType, safeMinimumOn, withDuration)
import Yesod.WebSockets (race_)

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

bestImageFile :: [Path Abs File] -> Maybe (Path Abs File)
bestImageFile = safeMinimumOn $ \path ->
  case lower . fromRelFile . fst <$> splitExtension (filename path) of
    Just "poster" -> 0 :: Int
    _ -> 100

data DirDiscovery = DirDiscovery
  { discoveredDir :: Path Abs Dir,
    discoveredSubDirs :: [Path Abs Dir],
    discoveredVideoFiles :: [Path Abs File],
    discoveredImageFiles :: [Path Abs File]
  }

data DirectoryKindGuess
  = DirectoryKindMovie Text -- Best guess for the movie title
  | DirectoryKindSeries Text -- Best guess for the series' name
  | DirectoryKindSeriesSeason -- For now we don't need any extra info about seasons
  | DirectoryKindUnknown

guessDirectoryKind :: DirDiscovery -> DirectoryKindGuess
guessDirectoryKind DirDiscovery {..} =
  let dirName = titleFromDir $ dirname discoveredDir
      dirSeasonIndicator = isJust $ seasonFromDir discoveredDir
      subDirsSeasonIndicators = any (isJust . seasonFromDir) discoveredSubDirs
      hasSubDirs = not $ null discoveredSubDirs
      filesSeasonIndicator = isJust $ seasonFromFiles discoveredVideoFiles
      hasMovieFiles = not $ null discoveredVideoFiles
   in firstJusts
        [ if dirSeasonIndicator
            then Just DirectoryKindSeriesSeason
            else Nothing,
          if subDirsSeasonIndicators && not hasMovieFiles
            then Just $ DirectoryKindSeries dirName
            else Nothing,
          if filesSeasonIndicator && not hasSubDirs
            then Just $ DirectoryKindSeries dirName
            else Nothing,
          if not filesSeasonIndicator && not hasSubDirs
            then Just $ DirectoryKindMovie dirName
            else Nothing
        ]
        `orElse` DirectoryKindUnknown

newtype DirToExplore = DirToExplore {unDirToExplore :: Path Abs Dir}

-- | This explores forever, so run async.
explorerThread ::
  -- Unbounded queue because this function reads one and then possible pushes multiple.
  -- I'm planning on only having a single thread do this, as the bottleneck is reading disk.
  -- We use a TQueue because I want to be able to send new stuff to discover from other threads too.
  -- Note: Since we're using a TQueue this means we'll be doing a breadth first search. I think that's good, but is not like a hard requirement.
  TQueue DirToExplore ->
  -- Here I'm not sure yet what is best, maybe a bounded queue is better, or maybe a TChan so potentially multiple threads can do stuff on discovery? Idk, we'll see what I need.
  TQueue DirDiscovery ->
  IO ()
explorerThread explorationQueue discoveryQueue = forever $ do
  DirToExplore nextDirToExplore <- atomically $ readTQueue explorationQueue
  -- Here we use `listDirectory` of the FilePath API, **NOT** `listDir` of the Path API. This is because `listDir` does a lot more IO to figure out what
  let dirPath = fromAbsDir nextDirToExplore
  withDuration (tryIO $ listDirectory dirPath) >>= \case
    (Right relPaths, duration) -> do
      let pathGuesses = map (guessPathType nextDirToExplore) relPaths
      let (subDirs, videoFiles, imageFiles, ignoredPaths) = partitionPathTypes pathGuesses
      -- TODO: Remember the ignored paths somewhere. Maybe keep a set of ignored paths somewhere and then show them in the interface? Could be helpful while debugging. For now just include some info about them in the log.
      putLog Info $
        concat
          [ "Explored directory ",
            dirPath,
            " (",
            show duration,
            "). Ignored files with these extensions: ",
            let nonIgnored = filter (not . isHiddenPath) ignoredPaths
             in showUnique (map takeExtension nonIgnored)
          ]
      atomically $ do
        forM_ subDirs $ writeTQueue explorationQueue . DirToExplore
        writeTQueue discoveryQueue $
          DirDiscovery
            { discoveredDir = nextDirToExplore,
              discoveredSubDirs = subDirs,
              discoveredVideoFiles = videoFiles,
              discoveredImageFiles = imageFiles
            }
    (Left err, duration) -> do
      case ioe_type err of
        InappropriateType ->
          -- This dir is actually a file.
          -- It's likely a wrong guess from a previous loop, so we can ignore it.
          -- Other option is that some other thread added it, and in both cases we probably want to know, so log.
          putLog Warning $
            concat
              [ "Tried exploring a directory that turned out to be a file ",
                dirPath,
                " (",
                show duration,
                ")"
              ]
        _ ->
          putLog Error $
            concat
              [ "Error while trying to explore a directory",
                dirPath,
                " (",
                show duration,
                "): ",
                displayException err
              ]

discoveryHandlerThread :: ConnectionPool -> Maybe TVDBToken -> TQueue DirDiscovery -> IO ()
discoveryHandlerThread dbConnPool tvdbToken discoveryQueue = forever $ do
  -- We're likely to be using a network share for the data, so walking this dir is slow.
  -- That means, discoveries comes slow, and we have plenty of time on each discovery to write to the locally stored DB.
  -- It also means that the locally stored DB might be partially updated during a full update of the data. But that's fine, we're not doing brain surgery here, it's fine to have imperfect data at times, we just want it quickly.
  dirDiscovery@DirDiscovery {..} <- atomically $ readTQueue discoveryQueue
  -- Time the work being done. Both the DB stuff and the potential followup action.
  ((), updateDur) <- withDuration $ do
    now <- getCurrentTime
    -- Try and do as much in a single transaction, but we might need some followup work that reads from disk and then does a second transaction
    (followupAction :: IO ()) <- runDBPool dbConnPool $ do
      -- Add dir, and if it already exists don't do anything.
      existingDir <- upsert (Directory discoveredDir Nothing Nothing) []

      -- Insert/update video files
      forM_ discoveredVideoFiles $ \filePath ->
        insertUnique_
          VideoFile
            { videoFileParent = entityKey existingDir,
              videoFileName = filename filePath,
              videoFileAdded = now,
              videoFileWatched = Nothing
            }

      -- TODO: Should we delete files that are no longe there?

      -- Figure out if we should do another update after.
      -- We do this in two steps because this followup action can then do slow IO, like reading the image from disk
      hasExistingImg <- hasImageQ discoveredDir
      -- Find best image on disc
      let mBestImg = bestImageFile discoveredImageFiles
      let followup :: IO ()
          followup = case (hasExistingImg, mBestImg) of
            (_, Just foundImg) -> do
              -- We found an image, so we update the directory in the db.
              -- Since this requires slow IO, we do this in the followup.
              -- It's an update, in case between this and the followup the directory got removed, it should then probably remain removed.
              imageData <- BS.readFile $ fromAbsFile foundImg
              void . runDBPool dbConnPool $
                update
                  (entityKey existingDir)
                  [ DirectoryImageContentType
                      =. Just (getImageContentType foundImg),
                    DirectoryImage =. Just imageData
                  ]
            (False, Nothing) -> do
              case tvdbToken of
                Nothing -> pure ()
                Just token -> do
                  -- There's no existing image in the DB, nor is there one on disk:
                  -- So we try and get one from the TVDB.
                  let mInfo = case guessDirectoryKind dirDiscovery of
                        DirectoryKindMovie title -> Just (title, TVDBTypeMovie)
                        DirectoryKindSeries title -> Just (title, TVDBTypeSeries)
                        DirectoryKindSeriesSeason -> Nothing
                        DirectoryKindUnknown -> Nothing
                  case mInfo of
                    Nothing -> pure ()
                    Just (name, type') -> do
                      -- If there were no images in disk, so we try and find one online.
                      mTVDBInfo <- getInfoFromTVDB token name type' Nothing Nothing
                      case mTVDBInfo >>= tvdbDataImage of
                        Nothing -> pure ()
                        Just (contentType, imageData) ->
                          void . runDBPool dbConnPool $
                            update
                              (entityKey existingDir)
                              [ DirectoryImageContentType =. Just contentType,
                                DirectoryImage =. Just imageData
                              ]
            (True, Nothing) ->
              -- We already have an image in the DB, just keep that one.
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
      [ "Got discovery and updated ",
        fromAbsDir discoveredDir,
        " (",
        show updateDur,
        ")"
      ]

-- | This creates multiple threads
dirUpdatorThreads ::
  ConnectionPool ->
  Maybe TVDBToken ->
  TQueue DirToExplore ->
  TQueue DirDiscovery ->
  IO ()
dirUpdatorThreads dbConnPool tvdbToken explorationQueue discoveryQueue = do
  race_
    (explorerThread explorationQueue discoveryQueue)
    (discoveryHandlerThread dbConnPool tvdbToken discoveryQueue)

-- Some helpers
showUnique :: [String] -> String
showUnique els = intercalate ", " $ Set.toList $ Set.fromList els

isHiddenPath :: FilePath -> Bool
isHiddenPath = \case
  '.' : _ -> True
  _ -> False

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
        ".aes",
        ".apk",
        ".bat",
        ".csv",
        ".db",
        ".db3-shm",
        ".db3-wal",
        ".db3",
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

-- | Compares taking into account numbers properly
naturalCompareBy :: (a -> String) -> a -> a -> Ordering
naturalCompareBy f a b = Natural.compare (lower $ f a) (lower $ f b)

-- | Sorts taking into account numbers properly
naturalSortBy :: (a -> String) -> [a] -> [a]
naturalSortBy f = sortBy $ naturalCompareBy f
