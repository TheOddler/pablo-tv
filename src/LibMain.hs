{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions
  ( Action (..),
    DirOrFile (..),
    KeyboardButton (..),
    MouseButton (..),
    actionsWebSocket,
    mkInputDevice,
    performAction,
  )
import Control.Exception (SomeException, displayException, try)
import Control.Monad (filterM, forM, forM_, when)
import Data.Aeson (Result (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace, toLower)
import Data.HashMap.Strict qualified as Map
import Data.List (foldl')
import Data.List.Extra (notNull, replace)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Directory
  ( DirectoryInfo (..),
    DirectoryKind (..),
    DirectoryRaw (..),
    TopLevelDir,
    getTopLevelDirs,
    getVideoDirPath,
    niceDirNameT,
    niceFileNameT,
    readAllDirectoryInfos,
    readDirectoryInfoRec,
    readDirectoryRaw,
    topLevelToAbsDir,
    unRootDir,
    updateAllDirectoryInfos,
    updateAllDirectoryInfosGuessOnly,
  )
import Evdev.Uinput (Device)
import Foreign.C (CTime (..))
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM, listToMaybe, orElse)
import GHC.MVar (MVar, newMVar)
import GHC.Utils.Misc (sortWith)
import IsDevelopment (isDevelopment)
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    dirname,
    fileExtension,
    fromAbsDir,
    fromAbsFile,
    mkRelDir,
    parent,
    parseAbsDir,
    parseAbsFile,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO (doesDirExist, doesFileExist, getHomeDir, listDir)
import Playerctl (Action (..), onFilePlayStarted)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, hasExtension)
import System.Posix (FileStatus, getFileStatus, isRegularFile)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBToken (..))
import TVState (TVState (..), addToAggWatched, startingTVState, tvStateWebSocket)
import Text.Hamlet (hamletFile)
import Util
  ( asyncOnTrigger,
    networkInterfaceWorthiness,
    removeLast,
    showIpV4OrV6WithPort,
    shuffle,
    toUrlRel,
    unsnoc,
    widgetFile,
  )
import Watched
  ( MarkAsWatchedResult (..),
    WatchedInfoAgg (..),
    hasBeenWatched,
    markFileAsWatched,
    readWatchedInfo,
    readWatchedInfoAgg,
  )
import Yesod hiding (defaultLayout, replace)
import Yesod qualified
import Yesod.EmbeddedStatic
import Yesod.WebSockets (race_, webSockets)

data App = App
  { appPort :: Int,
    appTVDBToken :: Maybe TVDBToken,
    appInputDevice :: Device,
    appTVState :: TVar TVState,
    appGetStatic :: EmbeddedStatic,
    appVideoDataRefreshTrigger :: MVar ()
  }

mkEmbeddedStatic
  False
  "embeddedStatic"
  [ embedFile "static/reconnecting-websocket.js",
    embedFile "static/fontawesome/css/all.min.css",
    embedFile "static/fontawesome/webfonts/fa-brands-400.ttf",
    embedFile "static/fontawesome/webfonts/fa-brands-400.woff2",
    embedFile "static/fontawesome/webfonts/fa-regular-400.ttf",
    embedFile "static/fontawesome/webfonts/fa-regular-400.woff2",
    embedFile "static/fontawesome/webfonts/fa-solid-900.ttf",
    embedFile "static/fontawesome/webfonts/fa-solid-900.woff2",
    embedFile "static/images/apple-tv-plus.png",
    embedFile "static/images/netflix.png",
    embedFile "static/images/youtube.png"
  ]

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
/ips AllIPsR GET
/input InputR GET
/remote RemoteR GET
/dir/+Texts DirectoryR GET

-- Other
/image/+Texts ImageR GET
/static StaticR EmbeddedStatic appGetStatic
|]

instance Yesod App where
  addStaticContent = embedStaticContent appGetStatic StaticR Right
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    isTv <- isTvRequest
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"

      when (isDevelopment && not isTv) $ do
        addScriptRemote "//cdn.jsdelivr.net/npm/eruda" -- Console for mobile
        toWidgetBody
          [julius|
            window.onload = function() {
              eruda.init();
            };
          |]

      networkInterfaces <- liftIO getNetworkInterfaces
      let mNetworkInterface =
            listToMaybe $
              sortWith networkInterfaceWorthiness networkInterfaces
      port <- getsYesod appPort
      inReadOnlyMode <- isNothing <$> getsYesod appTVDBToken

      addScript $ StaticR static_reconnecting_websocket_js
      addStylesheet $ StaticR static_fontawesome_css_all_min_css
      currentRoute <- fromMaybe HomeR <$> getCurrentRoute
      currentUrlBS <- toUrlRel currentRoute
      let currentUrl :: Text
          currentUrl = decodeUtf8Lenient $ BS.toStrict currentUrlBS
      $(widgetFile "default")
    withUrlRenderer $
      $(hamletFile "templates/page-wrapper.hamlet")

isTvRequest :: Handler Bool
isTvRequest = do
  mHostHeader <- lookupHeader "Host"
  let isHost h = maybe False (h `BS.isInfixOf`) mHostHeader
  pure $ any isHost ["localhost", "127.0.0.1", "0:0:0:0:0:0:0:1"]

defaultLayout :: Html -> Widget -> Handler Html
defaultLayout title widget = Yesod.defaultLayout $ do
  setTitle $ title <> " - Pablo TV"
  widget

type DirName = Text

type DirSegments = [Text]

type WatchedCount = Int

type TotalCount = Int

type DirData = (DirName, DirSegments, Maybe WatchedInfoAgg)

type ImageRoute = Route App

type Link = Text

type NamedLink = (Text, ImageRoute, Link)

data HomeSection
  = LocalVideos Text [DirData]
  | ExternalLinks Text [NamedLink]

getHomeR :: Handler Html
getHomeR = do
  -- This can be a websocket request, so do that
  tvStateTVar <- getsYesod appTVState
  inputDevice <- getsYesod appInputDevice
  videoDataRefreshTrigger <- getsYesod appVideoDataRefreshTrigger
  webSockets $
    race_
      (actionsWebSocket inputDevice tvStateTVar videoDataRefreshTrigger)
      (tvStateWebSocket tvStateTVar)

  -- Get proper data (we got this async from the state)
  tvState <- liftIO $ readTVarIO tvStateTVar
  videoDirPath <- liftIO getVideoDirPath
  topLevelDirs <- liftIO $ getTopLevelDirs videoDirPath
  let getDirData :: TopLevelDir -> DirData
      getDirData dir =
        let path = topLevelToAbsDir dir
            dirName = dirname path
            segments = fileNameToSegments dirName
            dirInfo = Map.lookup dir tvState.tvVideoData
         in ( maybe (niceDirNameT dirName) (directoryInfoTitle . fst) dirInfo,
              segments,
              snd <$> dirInfo
            )
  let videoData :: [DirData]
      videoData = getDirData <$> topLevelDirs
  let mkRandom =
        -- When in dev we auto-reload the page every second or so,
        -- so we want the same random shuffle every time, otherwise the page
        -- keeps changing which is annoying.
        if isDevelopment then pure (mkStdGen 2) else initStdGen
  randomGenerator <- mkRandom
  let isUnwatched (_, _, Nothing) = True
      isUnwatched (_, _, Just watched) = watched.watchedInfoPlayedVideoFileCount < watched.watchedInfoVideoFileCount
      unwatched = filter isUnwatched videoData
      recentlyAdded (_, _, Nothing) = CTime maxBound
      recentlyAdded (_, _, Just i) = (-i.watchedInfoLastModified)
      recentlyWatched (_, _, Nothing) = (posixSecondsToUTCTime 0, CTime minBound)
      recentlyWatched (_, _, Just i) = (i.watchedInfoLastWatched, i.watchedInfoLastAccessed)

  let sections =
        [ LocalVideos "New" $
            sortWith recentlyAdded unwatched,
          LocalVideos "Random" $
            shuffle unwatched randomGenerator,
          ExternalLinks
            "External Links"
            [ ("YouTube", StaticR static_images_youtube_png, "https://www.youtube.com/feed/subscriptions"),
              ("Netflix", StaticR static_images_netflix_png, "https://www.netflix.com"),
              ("Apple TV+", StaticR static_images_apple_tv_plus_png, "https://tv.apple.com")
            ],
          LocalVideos "Recently Added" $
            sortWith recentlyAdded videoData,
          LocalVideos "Random (All)" $
            shuffle videoData randomGenerator,
          LocalVideos "Recently Watched" . reverse $
            sortWith recentlyWatched videoData
        ]

  defaultLayout "Home" $(widgetFile "home")
  where
    fileNameToSegments :: Path Rel a -> [Text]
    fileNameToSegments f = [T.pack $ toFilePath f]

postHomeR :: Handler ()
postHomeR =
  parseCheckJsonBody >>= \case
    Error s -> do
      liftIO $ putStrLn $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Success action -> do
      inputDevice <- getsYesod appInputDevice
      tvStateTVar <- getsYesod appTVState
      videoDataRefreshTrigger <- getsYesod appVideoDataRefreshTrigger
      liftIO $ performAction inputDevice tvStateTVar videoDataRefreshTrigger action

-- | Turn the segments into a directory path and optionally a filename
-- Also checks if the file/directory actually exists, if not, return Nothing
parseSegments :: [Text] -> Handler (Maybe (Path Abs Dir, Maybe (Path Rel File)))
parseSegments segmentsT = do
  home <- liftIO getHomeDir
  let videoDirName = $(mkRelDir "Videos")
  case unsnoc segmentsT of
    Nothing -> pure $ Just (home </> videoDirName, Nothing)
    Just (parentSegments, dirOfFileT) -> do
      segments <- mapM parseRelDir (unpack <$> parentSegments)
      let parentPath = home </> foldl' (</>) videoDirName segments
          dirOfFile = T.unpack dirOfFileT
          dirName :: Maybe (Path Rel Dir)
          dirName = parseRelDir dirOfFile
          fileName :: Maybe (Path Rel File)
          fileName = parseRelFile dirOfFile

          tryDir =
            case dirName of
              Nothing -> pure Nothing
              Just dn -> do
                dirExists <- liftIO $ doesDirExist $ parentPath </> dn
                if dirExists
                  then pure $ Just (parentPath </> dn, Nothing)
                  else pure Nothing

      case fileName of
        Just fn -> do
          fileExists <- liftIO $ doesFileExist $ parentPath </> fn
          if fileExists
            then pure $ Just (parentPath, Just fn)
            else tryDir
        Nothing -> tryDir

getDirectoryR :: [Text] -> Handler Html
getDirectoryR segments = do
  absPath <-
    parseSegments segments >>= \case
      Just (p, Nothing) -> pure p
      _ -> redirect $ maybe HomeR DirectoryR $ removeLast segments

  mPathAndInfo <- liftIO $ readDirectoryInfoRec absPath
  let mInfo = snd <$> mPathAndInfo
  dirRaw <- liftIO $ readDirectoryRaw absPath

  let files :: [(Path Abs File, Text)]
      files = map (\f -> (absPath </> f, niceFileNameT f)) dirRaw.directoryVideoFiles

  -- Get the dirs info. Depending on where we are we do different things:
  -- \* At the root (segments is empty): We have the required data cached in the tv state. We do this because this folder is expected to have many subfolders.
  -- \* In a sub-dir: We read the required stuff from disk. We do this so we always have to most up-to-date data, and we don't expect too much stuff in here, so should be fine to read anew.
  dirs :: [(Path Rel Dir, Text, Maybe (Int, Int))] <- case segments of
    [] -> do
      tvState <- getsYesod appTVState >>= liftIO . readTVarIO
      videoDirPath <- liftIO getVideoDirPath
      topLevelDirs <- liftIO $ getTopLevelDirs videoDirPath
      let getDir :: TopLevelDir -> (Path Rel Dir, Text, Maybe (Int, Int))
          getDir dir =
            let dirName = dirname $ topLevelToAbsDir dir
                dirInfo = Map.lookup dir tvState.tvVideoData
             in ( dirName,
                  maybe (niceDirNameT dirName) (directoryInfoTitle . fst) dirInfo,
                  case snd <$> dirInfo of
                    Nothing -> Nothing
                    Just w ->
                      Just
                        ( w.watchedInfoPlayedVideoFileCount,
                          w.watchedInfoVideoFileCount
                        )
                )
      pure $ getDir <$> topLevelDirs
    _ -> do
      let getDir :: Path Rel Dir -> Handler (Path Rel Dir, Text, Maybe (Int, Int))
          getDir dirName = do
            watched <- liftIO $ readWatchedInfoAgg $ absPath </> dirName
            pure
              ( dirName,
                niceDirNameT dirName,
                Just
                  ( watched.watchedInfoPlayedVideoFileCount,
                    watched.watchedInfoVideoFileCount
                  )
              )
      mapM getDir dirRaw.directoryDirectories

  let mkSegments :: Path Rel x -> [Text]
      mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

      mkAbsFilePath :: Path Abs File -> String
      mkAbsFilePath filePath = replace "'" "\\'" $ fromAbsFile filePath

  watchedFiles <- liftIO $ readWatchedInfo absPath
  let watchedClassFile :: Path Abs File -> String
      watchedClassFile filePath =
        if hasBeenWatched watchedFiles filePath
          then "watched"
          else "unwatched"

      watchedClassDir :: Maybe (Int, Int) -> String
      watchedClassDir Nothing = "white"
      watchedClassDir (Just (watchedCount, totalCount)) =
        if watchedCount < totalCount
          then "unwatched"
          else "watched"

  let title = toHtml $ (directoryInfoTitle <$> mInfo) `orElse` "Videos"
  let showRefreshButton = null segments
  defaultLayout title $(widgetFile "directory")

getRemoteR :: Handler Html
getRemoteR = do
  defaultLayout "Remote" $(widgetFile "remote")

getImageR :: [Text] -> Handler Html
getImageR segments = do
  dir <-
    parseSegments segments >>= \case
      Just (d, _) -> pure d
      _ -> notFound
  mImg <- firstJustsM $ tryGetImage <$> take 3 (iterate parent dir)
  case mImg of
    Just (contentType, path) ->
      sendFile contentType $ fromAbsFile path
    Nothing ->
      notFound
  where
    tryGetImage :: Path Abs Dir -> Handler (Maybe (ContentType, Path Abs File))
    tryGetImage dir = do
      (_dirs, files) <- liftIO $ listDir dir
      let hasImageExt :: Path a File -> Bool
          hasImageExt file =
            let ext = fromMaybe "" (fileExtension file)
             in ext `elem` [".jpg", ".jpeg", ".png", ".gif"]
          isImage :: Path Abs File -> IO Bool
          isImage file =
            if not $ hasImageExt file
              then pure False
              else doesFileExist file
      mImageFile <- liftIO $ listToMaybe <$> filterM isImage files
      case mImageFile of
        Nothing -> pure Nothing
        Just absPath -> do
          ext <- fileExtension absPath
          let cleanedExt = map toLower $
                case ext of
                  '.' : e -> e
                  e -> e
          pure $ Just ("image/" <> fromString cleanedExt, absPath)

getInputR :: Handler Html
getInputR = do
  defaultLayout "Input" $(widgetFile "input")

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces' <- liftIO getNetworkInterfaces
  let networkInterfaces = sortWith networkInterfaceWorthiness networkInterfaces'
  port <- getsYesod appPort
  defaultLayout "IPs" $(widgetFile "ips")
  where
    hideZero :: (Show a, Eq a, Bounded a) => a -> String
    hideZero a =
      if a == minBound
        then ""
        else show a

withDuration :: IO a -> IO (a, NominalDiffTime)
withDuration f = do
  startTime <- getCurrentTime
  a <- f
  endTime <- getCurrentTime
  pure (a, diffUTCTime endTime startTime)

data DirInf = DirInf
  { dirPath :: Path Abs Dir,
    dirStatus :: FileStatus
  }

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

main :: IO ()
main = do
  videoDirPath <- getVideoDirPath
  ((dirs, _files, _ignored), totalTime) <-
    withDuration $
      walkDirBreadthFirst
        ( \path time -> do
            putStrLn $ "Walked " ++ show path ++ " in " ++ show time
        )
        (unRootDir videoDirPath)
  putStrLn $ "Reading recur took " ++ show totalTime
  putStrLn "Found these directories:"
  forM_ dirs print

  -- To get this token for now you can use your apiKey here https://thetvdb.github.io/v4-api/#/Login/post_login
  tvdbTokenRaw <- lookupEnv "TVDB_TOKEN"
  let tvdbToken = case tvdbTokenRaw of
        Nothing -> Nothing
        Just t | all isSpace t -> Nothing
        Just t -> Just $ TVDBToken $ BS.pack t
  when (isNothing tvdbToken) $
    putStrLn "No tvdb token found, so running in read-only mode."

  inputDevice <- mkInputDevice
  tvState <- newTVarIO startingTVState
  videoDataRefreshTrigger <- newMVar ()

  let port = 8080
  let (homePath, _params) = renderRoute HomeR
  let url = "http://localhost:" ++ show port ++ "/" ++ unpack (intercalate "/" homePath)
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    callProcess "xdg-open" [url]

  let dataUpdate :: IO [(TopLevelDir, DirectoryInfo)] -> IO ()
      dataUpdate infoGetter = do
        startTime <- getCurrentTime
        infosWithPath <- infoGetter
        let addFSInfo (path, info) = do
              infoFS <- readWatchedInfoAgg $ topLevelToAbsDir path
              pure (path, (info, infoFS))
        infos <- mapM addFSInfo infosWithPath
        atomically $ do
          state <- readTVar tvState
          writeTVar
            tvState
            state {tvVideoData = Map.fromList infos}
        endTime <- getCurrentTime
        putStrLn $ "Refreshed video data in " ++ show (diffUTCTime endTime startTime)

  -- Do an update once at startup, but only with already available data so we don't do any writes
  dataUpdate readAllDirectoryInfos

  -- In a thread do the data updating async
  let dataThread =
        asyncOnTrigger videoDataRefreshTrigger $
          dataUpdate $ case tvdbToken of
            Nothing -> updateAllDirectoryInfosGuessOnly
            Just t -> updateAllDirectoryInfos t

  -- The thread that'll be listening for files being played, and marking them as watched
  let watchedThread =
        onFilePlayStarted $ \case
          Nothing -> pure ()
          Just path -> do
            putStrLn $ "Playing file: " ++ show path
            result <- try $ markFileAsWatched path
            case result of
              Right AlreadyWatched -> pure ()
              Right MarkedAsWatched -> addToAggWatched tvState (parent path) 1
              Left (e :: SomeException) -> putStrLn $ "Failed marking as watched in thread: " ++ displayException e

  -- The thread for the app
  let appThread = do
        app <-
          toWaiAppPlain
            App
              { appPort = port,
                appTVDBToken = tvdbToken,
                appInputDevice = inputDevice,
                appTVState = tvState,
                appGetStatic = embeddedStatic,
                appVideoDataRefreshTrigger = videoDataRefreshTrigger
              }
        run port $ defaultMiddlewaresNoLogging app

  putStrLn "Starting race..."
  race_ appThread $
    race_ dataThread watchedThread

  putStrLn "Server quite."
