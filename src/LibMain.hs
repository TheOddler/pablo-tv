{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibMain where

import Actions
  ( Action (..),
    KeyboardButton (..),
    MouseButton (..),
    OverwritePreviouslyWatched (..),
    actionsWebSocket,
    markDirOrFileAsWatched,
    mkInputDevice,
    performAction,
  )
import Control.Exception (Exception (..))
import Control.Monad (when)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson qualified as Aeson
import Data.List.Extra (intercalate, notNull, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime)
import Data.Tuple.Extra (fst3)
import Directory
  ( AggDirInfo (..),
    findDirWithImageFor,
    getDirectoryAtPath,
    getMemoryFileDir,
    getSubDirAggInfo,
    loadRootsFromDisk,
    saveRootsToDisk,
  )
import Directory.Directories
  ( DirectoryData (..),
    DirectoryName (..),
    RootDirectories,
    RootDirectoryData (..),
    RootDirectoryLocation (..),
    rootDirectoryAsDirectory,
    rootDirectoryLocationToAbsPath,
    splitTitleFromDir,
  )
import Directory.Files
  ( VideoFileData (..),
    VideoFileName (..),
    getImageContentType,
    niceFileNameT,
  )
import Directory.Paths
  ( DirectoryPath (..),
    RawWebPath (..),
    VideoFilePath (..),
    rawWebPathFromDirectory,
    rawWebPathFromVideoFile,
    rawWebPathToDirectory,
    rootDirectoryPath,
  )
import Foundation
  ( App (..),
    Handler,
    Route (..),
    defaultLayout,
    embeddedStatic,
    resourcesApp,
    static_images_apple_tv_plus_png,
    static_images_netflix_png,
    static_images_youtube_png,
  )
import GHC.Conc (TVar, atomically, newTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Utils.Misc (sortWith)
import IsDevelopment (isDevelopment)
import Logging (LogLevel (..), LogMsg (..), Logger, mkLogSlidingWindow, putLog, readLogSlidingWindow)
import Mpris (MediaPlayer, MprisAction (..), getFirstMediaPlayer, mediaListener)
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import PVar (PVar, PVarState (..), modifyPVar_, newPVar, readPVar, readPVarState)
import SafeConvert (humanReadableBytes)
import SafeIO (SafeIO)
import Samba (MountResult, SmbServer (..), SmbShare (..), mount)
import System.Environment (getArgs)
import System.FilePath (pathSeparator)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVState (startingTVState, tvStateWebSocket)
import Transformers (LoggerT (..), SafeIOT (..), runLoggerT)
import UnliftIO (BufferMode (..), hSetBuffering, stderr, stdout, tryAny)
import Util
  ( logDuration,
    logOnErrorIO,
    naturalSortBy,
    networkInterfaceWorthiness,
    raceAll,
    showT,
    shuffle,
    unsnocNE,
    widgetFile,
  )
import Util.DirPath (absPathQQ, relPathQQ)
import Util.TextWithoutSeparator (splitAtSeparatorNE, unwrap)
import Yesod hiding (defaultLayout, replace)
import Yesod.WebSockets (race_, webSockets)

mkYesodDispatch "App" resourcesApp

data NamedLink = NamedLink
  { linkName :: Text,
    linkImage :: Route App,
    linkUrl :: Text
  }

data HomeSection
  = LocalVideos Text [AggDirInfo]
  | ExternalLinks Text [NamedLink]

getHomeR :: Handler Html
getHomeR = do
  -- This can be a websocket request, so do that
  tvStateTVar <- getsYesod appTVState
  webSockets $ race_ actionsWebSocket (tvStateWebSocket tvStateTVar)

  roots <- readPVar =<< getsYesod appRootDirs
  homeData <-
    logDuration "Calculated home dir agg data" . pure $
      concatMap
        ( \(rootLocation, rootData) ->
            getSubDirAggInfo
              (rootDirectoryPath rootLocation)
              (rootDirectoryAsDirectory rootData)
        )
        (Map.toList roots)

  let mkRandom =
        -- When in dev we auto-reload the page every second or so,
        -- so we want the same random shuffle every time, otherwise the page
        -- keeps changing which is annoying.
        if isDevelopment then pure (mkStdGen 2) else initStdGen
  randomGenerator <- mkRandom
  let isFinished d = aggDirPlayedVideoFileCount d == aggDirVideoFileCount d
      finished = filter isFinished homeData
      isUnstarted d = aggDirPlayedVideoFileCount d == 0
      unstarted = filter isUnstarted homeData
      isUnfinished d = aggDirPlayedVideoFileCount d < aggDirVideoFileCount d
      unfinished = filter isUnfinished homeData
      isWatching d = aggDirPlayedVideoFileCount d >= 1 && isUnfinished d
      watching = filter isWatching homeData
      recentlyAdded = Down . aggDirLastModified
      recentlyWatched = Down . aggDirLastWatched

  let sections =
        [ LocalVideos "Watching" $
            sortWith recentlyWatched watching,
          LocalVideos "New" $
            sortWith recentlyAdded unstarted,
          LocalVideos "Random" $
            shuffle unfinished randomGenerator,
          ExternalLinks
            "External Links"
            [ NamedLink
                "YouTube"
                (StaticR static_images_youtube_png)
                "https://www.youtube.com/feed/subscriptions",
              NamedLink
                "Netflix"
                (StaticR static_images_netflix_png)
                "https://www.netflix.com",
              NamedLink
                "Apple TV+"
                (StaticR static_images_apple_tv_plus_png)
                "https://tv.apple.com"
            ],
          LocalVideos "Recently Added" $
            sortWith recentlyAdded homeData,
          LocalVideos "Random (All)" $
            shuffle homeData randomGenerator,
          LocalVideos "Recently Finished" $
            sortWith recentlyWatched finished
        ]

  defaultLayout "Home" $(widgetFile "home")

postHomeR :: Handler ()
postHomeR =
  parseCheckJsonBody >>= \case
    Aeson.Error s -> do
      putLog Error $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Aeson.Success action -> do
      performAction action

data WatchState = Unwatched | Watching | Watched

watchedClass :: WatchState -> Html
watchedClass state = case state of
  Unwatched -> "unwatched"
  Watching -> "watching"
  Watched -> "watched"

fileWatchedClass :: VideoFileData -> Html
fileWatchedClass file =
  watchedClass $
    if isJust file.videoFileWatched
      then Watched
      else Unwatched

fileIcon :: VideoFileData -> Html
fileIcon file =
  if isJust file.videoFileWatched
    then "fa-solid fa-check"
    else ""

dirWatchedState :: AggDirInfo -> WatchState
dirWatchedState dirInfo =
  let total = aggDirVideoFileCount dirInfo
      watched = aggDirPlayedVideoFileCount dirInfo
   in if watched == 0
        then Unwatched
        else
          if watched == total
            then Watched
            else Watching

dirWatchedClass :: AggDirInfo -> Html
dirWatchedClass = watchedClass . dirWatchedState

dirIcon' :: WatchState -> Maybe Html
dirIcon' state = case state of
  Unwatched -> Nothing
  Watching -> Just "fa-solid fa-play"
  Watched -> Just "fa-solid fa-check"

dirIcon :: AggDirInfo -> Html
dirIcon = fromMaybe "" . dirIcon' . dirWatchedState

dirIconHome :: AggDirInfo -> Html
dirIconHome = fromMaybe "fa-regular fa-eye" . dirIcon' . dirWatchedState

type VideoFileWithNameAndPath = (VideoFileName, VideoFilePath, VideoFileData)

withDirectoryFromRaw :: (DirectoryPath -> Handler a) -> RawWebPath -> Handler a
withDirectoryFromRaw handler = withMDirectoryFromRaw $ \case
  Nothing -> notFound
  Just dirPath -> handler dirPath

withMDirectoryFromRaw :: (Maybe DirectoryPath -> Handler a) -> RawWebPath -> Handler a
withMDirectoryFromRaw handler rawWebPath = do
  roots <- readPVar =<< getsYesod appRootDirs
  handler $ rawWebPathToDirectory roots rawWebPath

getDirectoryR :: RawWebPath -> Handler Html
getDirectoryR = withMDirectoryFromRaw $ \mDirPath -> do
  roots <- readPVar =<< getsYesod appRootDirs
  (dirs, files) <- case mDirPath of
    Nothing ->
      pure
        ( naturalSortBy (unwrap . aggDirName) $
            concatMap
              ( \(rootLocation, rootData) ->
                  getSubDirAggInfo
                    (rootDirectoryPath rootLocation)
                    (rootDirectoryAsDirectory rootData)
              )
              (Map.toList roots),
          naturalSortBy (unwrap . fst3) $
            concatMap
              ( \(rootLocation, rootData) -> do
                  (videoName, videoData) <- Map.toList rootData.rootDirectoryVideoFiles
                  pure
                    ( videoName,
                      VideoFilePath rootLocation [] videoName,
                      videoData
                    )
              )
              (Map.toList roots)
        )
    Just dirPath -> do
      let mDir = getDirectoryAtPath roots dirPath
      dir <- case mDir of
        Nothing -> redirect $ DirectoryR $ RawWebPath []
        Just d -> pure d
      pure
        ( naturalSortBy (unwrap . aggDirName) $ getSubDirAggInfo dirPath dir,
          naturalSortBy (unwrap . fst3) $
            map
              ( \(videoName, videoData) ->
                  ( videoName,
                    VideoFilePath
                      dirPath.directoryPathRoot
                      dirPath.directoryPathNames
                      videoName,
                    videoData
                  )
              )
              (Map.toList dir.directoryVideoFiles)
        )

  let mDirPathRaw = rawWebPathFromDirectory <$> mDirPath
  let imagePath :: Maybe DirectoryPath
      imagePath = findDirWithImageFor roots =<< mDirPath
  let playAllAction = ActionPlayPath <$> mDirPathRaw
  let markAllWatchedAction = ActionMarkAsWatched <$> mDirPathRaw
  let markAllUnwatchedAction = ActionMarkAsUnwatched <$> mDirPathRaw
  let refreshDirectoryLabelAndAction =
        case mDirPathRaw of
          Nothing ->
            Just
              ( "Refresh Library" :: String,
                ActionRefreshAllDirectoryData
              )
          Just dirPathRaw ->
            Just
              ( "Refresh this directory" :: String,
                ActionRefreshDirectoryData dirPathRaw
              )
  let (title, subTitle) = case NE.nonEmpty . directoryPathNames <$> mDirPath of
        Nothing -> ("Videos", "")
        Just Nothing -> ("Videos", "")
        Just (Just ne) -> splitTitleFromDir $ NE.last ne
  defaultLayout (toHtml title) $(widgetFile "directory")

getImageR :: RawWebPath -> Handler TypedContent
getImageR = withDirectoryFromRaw $ \dirPath -> do
  roots <- readPVar =<< getsYesod appRootDirs
  -- We find the directory with the image for a path in the directory endpoint,
  -- so here we just have to check the directory itself, not its parents.
  -- This makes sure that the browser gets a consistent path and can do better caching.
  let mImg = getDirectoryAtPath roots dirPath >>= directoryImage
  case mImg of
    Nothing -> notFound
    Just (imgName, imgBytes) -> do
      addHeader "Cache-Control" "max-age=604800, public" -- Cache 1 week
      sendResponse (getImageContentType imgName, toContent imgBytes)

getRemoteR :: Handler Html
getRemoteR = do
  defaultLayout "Remote" $(widgetFile "remote")

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

getDebugR :: Handler Html
getDebugR = do
  app <- getYesod
  rootDirPVarState' <- readPVarState app.appRootDirs
  let rootDirPVarState = case rootDirPVarState' of
        PVarReady -> "Ready" :: Text
        PVarUpdating desc -> "Updating: " <> desc

  runtimeStats <- liftIO $ tryAny getRTSStats

  logMsgs <- readLogSlidingWindow app.appLogSlidingWindow
  let logClass :: LogMsg -> String
      logClass msg = case msg.logMsgLevel of
        Debug -> "debug"
        Info -> "info"
        Warning -> "warning"
        Error -> "error"
  let prettyLogTime :: LogMsg -> String
      prettyLogTime msg = formatTime defaultTimeLocale "%H:%M:%S" msg.logMsgTime

  defaultLayout "Debug" $(widgetFile "debug")

mountAllSambaShares :: (SafeIO m, Logger m) => RootDirectories -> m ()
mountAllSambaShares roots = do
  let sambaShares =
        mapMaybe
          ( \case
              RootRelToHome _ -> Nothing
              RootAbsPath _ -> Nothing
              RootSamba srv shr -> Just (srv, shr)
          )
          (Map.keys roots)
  results <- mapM (uncurry mount) sambaShares
  let showResult :: ((SmbServer, SmbShare), MountResult) -> String
      showResult ((SmbServer srv, SmbShare shr), result) =
        srv ++ "/" ++ shr ++ ": " ++ show result
  putLog Info $
    "Mounted sambas:\n\t"
      ++ intercalate "\t\n" (showResult <$> zip sambaShares results)

mediaListenerHandler :: (SafeIO m, Logger m) => PVar RootDirectories -> FilePath -> m ()
mediaListenerHandler rootDirsPVar absFilePath = do
  modifyPVar_ rootDirsPVar ("Media listener handler: " <> showT absFilePath) $ \roots -> do
    let tryRoot rootLoc = do
          rootAbsPath <- rootDirectoryLocationToAbsPath rootLoc
          let mRest = stripPrefix (rootAbsPath ++ [pathSeparator]) absFilePath
          case mRest of
            Nothing -> pure Nothing
            Just rest -> do
              let pathPieces = splitAtSeparatorNE $ T.pack rest
              let (dirNames', fileName) = unsnocNE pathPieces
              let dirNames = DirectoryName <$> dirNames'
              -- We'll pretend that this filename is always a video file, if not we just won't find the file and not update it, so that's fine too.
              let videoFileName = VideoFileName fileName
              pure $ Just $ VideoFilePath rootLoc dirNames videoFileName
    mPath <- firstJustsM $ tryRoot <$> Map.keys roots
    case mPath of
      Nothing -> do
        putLog Info $ "Path did not match any known files, so didn't mark any file as watched: " ++ absFilePath
        pure roots
      Just path -> do
        putLog Info $ "Marking file as watched: " ++ show path
        markDirOrFileAsWatched OverwritePreviouslyWatched (Right path) roots
  saveRootsToDisk rootDirsPVar

playerListenerHandler :: (MonadIO m) => TVar (Maybe MediaPlayer) -> MediaPlayer -> m ()
playerListenerHandler lastActivePlayerTVar playerName = do
  liftIO $ atomically $ writeTVar lastActivePlayerTVar $ Just playerName

main :: IO ()
main = do
  args <- getArgs
  let minLogLevel =
        if "--log-debug" `elem` args
          then Debug
          else Info
  let logBuffering =
        -- By default Haskell does block buffering, but I want line buffering by default
        if "--log-block-buffering" `elem` args
          then BlockBuffering Nothing
          else LineBuffering
  logWindow <- mkLogSlidingWindow 1000
  runLoggerT logWindow minLogLevel . runSafeIOT $ do
    logOnErrorIO Error ("setting log buffering stdout to " ++ show logBuffering) $
      hSetBuffering stdout logBuffering
    logOnErrorIO Error ("setting log buffering stdout to " ++ show logBuffering) $
      hSetBuffering stderr logBuffering

    putLog Info $ "Terminal arguments: " ++ show args
    putLog Info $ "Min log level: " ++ show minLogLevel

    let port = 8080
    let (homePath, _params) = renderRoute HomeR
    let url = "http://localhost:" ++ show port ++ "/" ++ unpack (Text.intercalate "/" homePath)
    putLog Info $ "Running on port " ++ show port ++ " - " ++ url
    putLog Info $ "Development mode: " ++ show isDevelopment

    memoryFileDir <- getMemoryFileDir
    putLog Info $ "Memory file(s) directory: " ++ memoryFileDir

    mRootDirs <- loadRootsFromDisk
    let emptyRootData = RootDirectoryData Map.empty Map.empty
    let rootDirsDefault =
          Map.fromList
            [ (RootRelToHome [relPathQQ|Videos|], emptyRootData),
              (RootAbsPath [absPathQQ|/home/pablo/Downloads/Torrents|], emptyRootData),
              -- TODO: I'll have to add an interface somewhere to add these
              (RootSamba (SmbServer "192.168.0.99") (SmbShare "videos"), emptyRootData)
            ]
    let rootDirs = fromMaybe rootDirsDefault mRootDirs
    rootDirsPVar <- newPVar rootDirs

    -- Open samba shares. TODO: Make some way of checking and re-mounting the broken ones at runtime
    mountAllSambaShares rootDirs

    -- Get the log func
    (logFunc, _) <- SafeIOT $ LoggerT ask

    -- Create input device
    inputDevice <- mkInputDevice

    -- Create empty tv state
    tvState <- liftIO $ newTVarIO startingTVState

    -- Get Mpris stuff
    player <- getFirstMediaPlayer
    lastActivePlayerTVar <- liftIO $ newTVarIO player

    -- The thread for the app
    let app =
          App
            { appPort = port,
              appLogFunc = logFunc,
              appLogSlidingWindow = logWindow,
              appInputDevice = inputDevice,
              appTVState = tvState,
              appGetStatic = embeddedStatic,
              appLastActivePlayer = lastActivePlayerTVar,
              appRootDirs = rootDirsPVar
            }
    let appThread = toWaiAppPlain app >>= run port . defaultMiddlewaresNoLogging

    -- The thread that'll be listening for files being played, and marking them as watched
    -- This function returns instantly, but in the background it's still running, so no need to race
    _ <-
      mediaListener
        (mediaListenerHandler rootDirsPVar)
        (playerListenerHandler lastActivePlayerTVar)

    -- Only open the browser automatically in production because it's annoying in
    -- development as it opens a new tab every time the server restarts.
    -- Do this as late as possible, because if we're too fast the server won't actually respond yet.
    when (not isDevelopment) . logOnErrorIO Warning "opening pablo-tv home" $
      callProcess "xdg-open" [url]

    putLog Info "Starting server..."
    liftIO $ raceAll [appThread]

    putLog Debug "Server quit."
