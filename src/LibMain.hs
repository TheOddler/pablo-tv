{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibMain where

import Actions
  ( Action (..),
    DirOrFile (..),
    KeyboardButton (..),
    MouseButton (..),
    actionsWebSocket,
    mkInputDevice,
    performAction,
    performActionIO,
  )
import Control.Concurrent.STM (newTQueueIO)
import Control.Monad (forM, when)
import DB
  ( AggDirInfo (..),
    EntityField (..),
    Key (..),
    SambaShare (..),
    VideoFile (..),
    getAggSubDirsInfoQ,
    getAllRootDirectories,
    getImageQ,
    hasImageQ,
    migrateAll,
    runDBPool,
  )
import Data.Aeson qualified as Aeson
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Extra (notNull)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.Persist.Sqlite
  ( ConnectionPool,
    extraPragmas,
    mkSqliteConnectionInfo,
    runMigration,
    withSqlitePoolInfo,
  )
import Directory
  ( dirUpdatorThreads,
    naturalSortBy,
    niceDirNameT,
    niceFileNameT,
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
import GHC.Conc (newTVarIO)
import GHC.Utils.Misc (sortWith)
import IsDevelopment (isDevelopment)
import Lens.Micro ((&), (.~))
import Logging (LogLevel (..), logDuration, putLog, runLoggingT)
import Mpris (MprisAction (..))
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path (Abs, Dir, File, Path, dirname, fromRelDir, fromRelFile, (</>))
import Playerctl (onFilePlayStarted)
import Samba (MountResult, SmbServer (..), SmbShare (..), mount)
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBApiKey (..), getToken)
import TVState (startingTVState, tvStateWebSocket)
import Util
  ( networkInterfaceWorthiness,
    raceAll,
    shuffle,
    widgetFile,
  )
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

  homeData <-
    logDuration "Queried DB for home data" . runDB $ do
      allRootDirs <- getAllRootDirectories
      concat <$> forM allRootDirs getAggSubDirsInfoQ

  let mkRandom =
        -- When in dev we auto-reload the page every second or so,
        -- so we want the same random shuffle every time, otherwise the page
        -- keeps changing which is annoying.
        if isDevelopment then pure (mkStdGen 2) else initStdGen
  randomGenerator <- mkRandom
  let isUnwatched d = aggDirPlayedVideoFileCount d < aggDirVideoFileCount d
      unwatched = filter isUnwatched homeData
      recentlyAdded d = Down $ aggDirLastModified d
      recentlyWatched = aggDirLastWatched

  let sections =
        [ LocalVideos "New" $
            sortWith recentlyAdded unwatched,
          LocalVideos "Random" $
            shuffle unwatched randomGenerator,
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
          LocalVideos "Recently Watched" . reverse $
            sortWith recentlyWatched homeData
        ]

  defaultLayout "Home" $(widgetFile "home")

postHomeR :: Handler ()
postHomeR =
  parseCheckJsonBody >>= \case
    Aeson.Error s -> do
      liftIO $ putLog Error $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Aeson.Success action -> do
      performAction action

watchedClass :: Bool -> Html
watchedClass watched = if watched then "watched" else "unwatched"

watchedClassM :: Maybe UTCTime -> Html
watchedClassM = watchedClass . isJust

isWatchedDir :: AggDirInfo -> Bool
isWatchedDir dirInfo = aggDirPlayedVideoFileCount dirInfo >= aggDirVideoFileCount dirInfo

watchedClassDir :: AggDirInfo -> Html
watchedClassDir dirInfo =
  watchedClass $ isWatchedDir dirInfo

videoFileAbsPath :: VideoFile -> Path Abs File
videoFileAbsPath f =
  let DirectoryKey parentPath = videoFileParent f
   in parentPath </> videoFileName f

getDirectoryHomeR :: Handler Html
getDirectoryHomeR = do
  (dirs' :: [AggDirInfo], files' :: [VideoFile]) <-
    logDuration "Queried DB for home data" . runDB $ do
      allRootDirs <- getAllRootDirectories
      ds <- concat <$> forM allRootDirs getAggSubDirsInfoQ
      fs <-
        concat
          <$> forM
            allRootDirs
            ( \rootDir ->
                selectList [VideoFileParent ==. DirectoryKey rootDir] []
            )
      pure (ds, map entityVal fs)

  let dirs = naturalSortBy (fromRelDir . dirname . aggDirPath) dirs'
      files = naturalSortBy (fromRelFile . videoFileName) files'

  -- We share the widget with directory, so we need these, but they are all hidden on the home dir
  let imagePath = Nothing
  let playAllAction = Nothing :: Maybe Actions.Action
  let markAllWatchedAction = Nothing :: Maybe Actions.Action
  let markAllUnwatchedAction = Nothing :: Maybe Actions.Action
  let refreshDirectoryLabelAndAction =
        Just
          ( "Refresh Library" :: String,
            ActionRefreshAllDirectoryData
          )
  let title = "Videos"
  defaultLayout title $(widgetFile "directory")

getDirectoryR :: Path Abs Dir -> Handler Html
getDirectoryR dirPath = do
  (dirs' :: [AggDirInfo], files' :: [VideoFile], hasImage) <- logDuration "Get child dirs and files" $ runDB $ do
    ds <- getAggSubDirsInfoQ dirPath
    fs <- selectList [VideoFileParent ==. DirectoryKey dirPath] []
    hasImage <- hasImageQ dirPath
    pure (ds, map entityVal fs, hasImage)

  let dirs = naturalSortBy (fromRelDir . dirname . aggDirPath) dirs'
      files = naturalSortBy (fromRelFile . videoFileName) files'

  let imagePath = if hasImage then Just dirPath else Nothing
  let playAllAction = Just $ ActionPlayPath $ Dir dirPath
  let markAllWatchedAction = Just $ ActionMarkAsWatched $ Dir dirPath
  let markAllUnwatchedAction = Just $ ActionMarkAsUnwatched $ Dir dirPath
  let refreshDirectoryLabelAndAction =
        Just
          ( "Refresh this directory" :: String,
            ActionRefreshDirectoryData dirPath
          )
  let title = toHtml $ niceDirNameT dirPath
  defaultLayout title $(widgetFile "directory")

getImageR :: Path Abs Dir -> Handler Html
getImageR absPath =
  runDB (getImageQ absPath) >>= \case
    Nothing -> notFound
    Just (imgContentType, imgBytes) -> sendResponse (imgContentType, toContent imgBytes)

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

mountAllSambaShares :: ConnectionPool -> IO ()
mountAllSambaShares connPool = do
  sambaShares <- runDBPool connPool $ selectList [] []
  let doMount (SambaShare svr shr) = mount svr shr
  results <- forM sambaShares $ doMount . entityVal
  let showResult :: (SambaShare, MountResult) -> String
      showResult (SambaShare (SmbServer srv) (SmbShare shr), result) =
        srv ++ "/" ++ shr ++ ": " ++ show result
  putLog Info $
    "Mounted sambas:\n\t"
      ++ intercalate "\t\n" (showResult <$> zip (entityVal <$> sambaShares) results)

main :: IO ()
main = do
  -- To get this token for now you can use your apiKey here https://thetvdb.github.io/v4-api/#/Login/post_login
  tvdbApiKeyRaw <- lookupEnv "TVDB_API_KEY"
  let tvdbApiKey = case tvdbApiKeyRaw of
        Nothing -> Nothing
        Just str | all isSpace str -> Nothing
        Just str -> Just $ TVDBApiKey str
  tvdbToken <- case tvdbApiKey of
    Nothing -> pure Nothing
    Just key -> getToken key
  when (isNothing tvdbToken) $
    putLog Info "No tvdb token found, so running in read-only mode."

  inputDevice <- mkInputDevice
  tvState <- newTVarIO startingTVState
  dirExplorationQueue <- newTQueueIO
  dirDiscoveryQueue <- newTQueueIO

  let port = 8080
  let (homePath, _params) = renderRoute HomeR
  let url = "http://localhost:" ++ show port ++ "/" ++ unpack (Text.intercalate "/" homePath)
  putLog Info $ "Running on port " ++ show port ++ " - " ++ url
  putLog Info $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    callProcess "xdg-open" [url]

  let openConnectionCount = 10
  let connectionInfo =
        mkSqliteConnectionInfo "pablo-tv-data.db3"
          -- & walEnabled .~ True -- The default
          -- & fkEnabled .~ True -- The default
          & extraPragmas .~ ["PRAGMA busy_timeout = 30000"]
  runLoggingT $
    withSqlitePoolInfo connectionInfo openConnectionCount $ \connPool -> liftIO $ do
      -- Migrate DB
      logDuration "Migration" $ runDBPool connPool $ runMigration migrateAll

      -- Open samba shares. TODO: Make some way of checking and re-mounting the broken ones at runtime
      mountAllSambaShares connPool

      -- The thread for the app
      let app =
            App
              { appPort = port,
                appTVDBToken = tvdbToken,
                appInputDevice = inputDevice,
                appTVState = tvState,
                appSqlPool = connPool,
                appGetStatic = embeddedStatic,
                appDirExplorationQueue = dirExplorationQueue
              }
      let appThread = toWaiAppPlain app >>= run port . defaultMiddlewaresNoLogging

      -- The thread that'll be listening for files being played, and marking them as watched
      let watchedThread =
            onFilePlayStarted $ \case
              Nothing -> pure ()
              Just path -> do
                putLog Info $ "Heard file playing: " ++ show path
                performActionIO app $ ActionMarkAsWatched $ File path

      putLog Info "Starting server..."
      raceAll
        [ appThread,
          watchedThread,
          dirUpdatorThreads connPool tvdbToken dirExplorationQueue dirDiscoveryQueue
        ]

  putLog Debug "Server quit."
