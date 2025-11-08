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
import Control.Concurrent (newMVar, readMVar)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Char (isSpace)
import Data.List.Extra (notNull)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Directory
  ( AggDirInfo (..),
    Directory (..),
    DirectoryCheckResult (..),
    DirectoryData (..),
    DirectoryName,
    DirectoryPath (..),
    RootDirectory (..),
    RootDirectoryType (..),
    VideoFile (..),
    VideoFileData (..),
    VideoFilePath (..),
    checkDirectory,
    getDirectoryData,
    getSubDirAggInfo,
    niceDirNameT,
    niceFileNameT,
    rootDirectoryAsDirectory,
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
import Mpris (MprisAction (..), mediaListener)
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
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

  roots <- liftIO . readMVar =<< getsYesod appRootDirs
  let homeData :: [AggDirInfo]
      homeData = concatMap (getSubDirAggInfo . rootDirectoryAsDirectory) roots

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

getDirectoryHomeR :: Handler Html
getDirectoryHomeR = do
  roots <- liftIO . readMVar =<< getsYesod appRootDirs
  let dirs :: [AggDirInfo]
      dirs = concatMap (getSubDirAggInfo . rootDirectoryAsDirectory) roots
  let files :: [VideoFile]
      files =
        concatMap
          ( \r -> do
              let vids = Map.assocs r.rootDirectoryData.directoryVideoFiles
              let dirPath = DirectoryPath r.rootDirectoryType []
              uncurry (VideoFile . VideoFilePath dirPath) <$> vids
          )
          roots

  -- We share the widget with directory, so we need these, but they are all hidden on the home dir
  let imagePath = Nothing :: Maybe DirectoryPath
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

getDirectoryR :: RootDirectoryType -> [DirectoryName] -> Handler Html
getDirectoryR rootType dirNames = do
  roots <- liftIO . readMVar =<< getsYesod appRootDirs
  dirNamesNE <- case dirNames of
    [] -> redirect DirectoryHomeR
    f : r -> pure $ f NE.:| r
  let dirPath = DirectoryPath rootType dirNames
  let mDirData = getDirectoryData roots dirPath
  dirData <- case mDirData of
    Nothing -> redirect DirectoryHomeR
    Just d -> pure d
  let dirs :: [AggDirInfo]
      dirs = getSubDirAggInfo $ Directory dirPath dirData
  let files :: [VideoFile]
      files = uncurry (VideoFile . VideoFilePath dirPath) <$> Map.assocs dirData.directoryVideoFiles

  let imagePath = if isJust dirData.directoryImage then Just dirPath else Nothing
  let playAllAction = Just $ ActionPlayPath $ Dir dirPath
  let markAllWatchedAction = Just $ ActionMarkAsWatched $ Dir dirPath
  let markAllUnwatchedAction = Just $ ActionMarkAsUnwatched $ Dir dirPath
  let refreshDirectoryLabelAndAction =
        -- Just
        --   ( "Refresh this directory" :: String,
        --     ActionRefreshDirectoryData dirPath
        --   )
        -- TODO: I need to re-enabled ActionRefreshDirectoryData
        Nothing :: Maybe (String, Action)
  let title = toHtml $ niceDirNameT $ NE.last dirNamesNE
  defaultLayout title $(widgetFile "directory")

getImageR :: RootDirectoryType -> [DirectoryName] -> Handler TypedContent
getImageR root dirNames = notFound -- TODO
-- runDB (getImageQ absPath) >>= \case
--   Nothing -> notFound
--   Just (imgContentType, imgBytes) -> do
--     addHeader "Cache-Control" "max-age=604800, public" -- Cache 1 week
--     sendResponse (imgContentType, toContent imgBytes)

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

mountAllSambaShares :: IO ()
mountAllSambaShares = do
  -- sambaShares <- runDBPool connPool $ selectList [] []
  -- let doMount (SambaShare svr shr) = mount svr shr
  -- results <- forM sambaShares $ doMount . entityVal
  -- let showResult :: (SambaShare, MountResult) -> String
  --     showResult (SambaShare (SmbServer srv) (SmbShare shr), result) =
  --       srv ++ "/" ++ shr ++ ": " ++ show result
  -- putLog Info $
  --   "Mounted sambas:\n\t"
  --     ++ intercalate "\t\n" (showResult <$> zip (entityVal <$> sambaShares) results)
  -- TODO: Actually implement this
  pure ()

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

  let port = 8080
  let (homePath, _params) = renderRoute HomeR
  let url = "http://localhost:" ++ show port ++ "/" ++ unpack (Text.intercalate "/" homePath)
  putLog Info $ "Running on port " ++ show port ++ " - " ++ url
  putLog Info $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    callProcess "xdg-open" [url]

  rootDirs <- logDuration "Startup directory check" $ do
    result <-
      checkDirectory $
        Directory
          (DirectoryPath RootLocalVideos [])
          (DirectoryData mempty Nothing mempty)
    let dirData = case result of
          DirectoryUnchanged -> DirectoryData mempty Nothing mempty
          DirectoryChanged d -> d
          DirectoryNotADirectory -> DirectoryData mempty Nothing mempty
    newMVar [RootDirectory RootLocalVideos dirData]

  runLoggingT $ liftIO $ do
    -- Open samba shares. TODO: Make some way of checking and re-mounting the broken ones at runtime
    mountAllSambaShares

    -- The thread for the app
    let app =
          App
            { appPort = port,
              appTVDBToken = tvdbToken,
              appInputDevice = inputDevice,
              appTVState = tvState,
              appGetStatic = embeddedStatic,
              appRootDirs = rootDirs
            }
    let appThread = toWaiAppPlain app >>= run port . defaultMiddlewaresNoLogging

    -- The thread that'll be listening for files being played, and marking them as watched
    -- This is function returns instantly, but in the background it's still running, so no need to race
    _ <- mediaListener $ \path -> do
      putLog Info $ "Heard file playing: " ++ show path
      performActionIO app $ ActionMarkAsWatched $ File path

    putLog Info "Starting server..."
    raceAll [appThread]

  putLog Debug "Server quit."
