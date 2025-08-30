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
import Control.Monad (when)
import DB (AggDirInfo (..), EntityField (..), Key (..), VideoFile (..), getAggSubDirInfoQ, getNearestImage, migrateAll, runDBPool)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List.Extra (notNull)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.Persist.Sqlite (runMigration, withSqlitePool)
import Directory (getVideoDirPath, naturalSortBy, niceDirNameT, niceFileNameT, updateData)
import Foundation (App (..), Handler, Route (..), defaultLayout, embeddedStatic, resourcesApp, static_images_apple_tv_plus_png, static_images_netflix_png, static_images_youtube_png)
import GHC.Conc (newTVarIO)
import GHC.MVar (newMVar)
import GHC.Utils.Misc (sortWith)
import IsDevelopment (isDevelopment)
import Logging (LogLevel (..), logDuration, putLog, runLoggingT)
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    dirname,
    fromRelDir,
    fromRelFile,
    (</>),
  )
import Playerctl (Action (..), onFilePlayStarted)
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBToken (..))
import TVState (startingTVState, tvStateWebSocket)
import Util
  ( getImageContentType,
    networkInterfaceWorthiness,
    shuffle,
    widgetFile,
  )
import Yesod hiding (defaultLayout, replace)
import Yesod.WebSockets (concurrently_, race_, webSockets)

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

  homeDir <- liftIO getVideoDirPath -- For now we just use the hardcoded path as home, but the plan is to support multiple root folders
  homeData <-
    logDuration "Queried DB for home data" . runDB $
      getAggSubDirInfoQ homeDir

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

getDirectoryR :: Path Abs Dir -> Handler Html
getDirectoryR absPath = do
  (dirs' :: [AggDirInfo], files' :: [VideoFile]) <- logDuration "Get child dirs and files" $ runDB $ do
    ds <- getAggSubDirInfoQ absPath
    fs <- selectList [VideoFileParent ==. DirectoryKey absPath] []
    pure (ds, map entityVal fs)

  let dirs = naturalSortBy (fromRelDir . dirname . aggDirPath) dirs'
      files = naturalSortBy (fromRelFile . videoFileName) files'

  let watchedClass :: Bool -> Html
      watchedClass watched = if watched then "watched" else "unwatched"
      watchedClassM :: Maybe UTCTime -> Html
      watchedClassM = watchedClass . isJust
      isWatchedDir :: AggDirInfo -> Bool
      isWatchedDir dirInfo = aggDirPlayedVideoFileCount dirInfo >= aggDirVideoFileCount dirInfo
      watchedClassDir :: AggDirInfo -> Html
      watchedClassDir dirInfo =
        watchedClass $ isWatchedDir dirInfo

  let videoFileAbsPath :: VideoFile -> Path Abs File
      videoFileAbsPath f = absPath </> videoFileName f

  let title = toHtml $ niceDirNameT absPath
  defaultLayout title $(widgetFile "directory")

getImageR :: Path Abs Dir -> Handler Html
getImageR absPath =
  runDB (getNearestImage absPath) >>= \case
    Nothing -> notFound
    Just (imgName, imgBytes) -> sendResponse (getImageContentType imgName, toContent imgBytes)

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

main :: IO ()
main = do
  -- To get this token for now you can use your apiKey here https://thetvdb.github.io/v4-api/#/Login/post_login
  tvdbTokenRaw <- lookupEnv "TVDB_TOKEN"
  let tvdbToken = case tvdbTokenRaw of
        Nothing -> Nothing
        Just t | all isSpace t -> Nothing
        Just t -> Just $ TVDBToken $ BS.pack t
  when (isNothing tvdbToken) $
    putLog Info "No tvdb token found, so running in read-only mode."

  inputDevice <- mkInputDevice
  tvState <- newTVarIO startingTVState
  videoDataRefreshTrigger <- newMVar ()

  let port = 8080
  let (homePath, _params) = renderRoute HomeR
  let url = "http://localhost:" ++ show port ++ "/" ++ unpack (intercalate "/" homePath)
  putLog Info $ "Running on port " ++ show port ++ " - " ++ url
  putLog Info $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    callProcess "xdg-open" [url]

  let openConnectionCount = 1 -- If we increase this, you should also change the connection string to allow for concurrent read/writes
  runLoggingT $
    withSqlitePool "pablo-tv-data.db3" openConnectionCount $ \connPool -> liftIO $ do
      -- Migrate DB
      logDuration "Migration" $ runDBPool connPool $ runMigration migrateAll

      let dataThread = do
            -- Update data, eventually I want to do this on a separate thread
            videoDirPath <- getVideoDirPath
            updateData connPool videoDirPath

      -- Start the rest of the server
      let app =
            App
              { appPort = port,
                appTVDBToken = tvdbToken,
                appInputDevice = inputDevice,
                appTVState = tvState,
                appSqlPool = connPool,
                appGetStatic = embeddedStatic,
                appVideoDataRefreshTrigger = videoDataRefreshTrigger
              }

      -- The thread for the app
      let appThread = toWaiAppPlain app >>= run port . defaultMiddlewaresNoLogging

      -- The thread that'll be listening for files being played, and marking them as watched
      let watchedThread =
            onFilePlayStarted $ \case
              Nothing -> pure ()
              Just path -> do
                putLog Info $ "Heard file playing: " ++ show path
                performActionIO app $ ActionMarkAsWatched $ File path

      putLog Info "Starting server..."
      concurrently_ dataThread $
        race_ appThread watchedThread

  putLog Debug "Server quit."
