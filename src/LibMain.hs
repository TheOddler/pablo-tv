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
import DB (Directory (..), EntityField (..), VideoFile (..), migrateAll, runDBWithConn)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (foldl')
import Data.List.Extra (notNull)
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql.Raw.QQ (sqlQQ)
import Database.Persist.Sqlite (Single (..), runMigration, withSqlitePool)
import Directory (getVideoDirPath, niceDirNameT, niceFileNameT, updateData)
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
    Rel,
    mkRelDir,
    parseRelDir,
    parseRelFile,
    (</>),
  )
import Path.IO (doesDirExist, doesFileExist, getHomeDir)
import Playerctl (Action (..), onFilePlayStarted)
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBToken (..))
import TVState (startingTVState, tvStateWebSocket)
import Util
  ( fst5,
    getImageContentType,
    networkInterfaceWorthiness,
    shuffle,
    unSingle2,
    unSingle5,
    uncurry5,
    unsnoc,
    widgetFile,
  )
import Yesod hiding (defaultLayout, replace)
import Yesod.WebSockets (concurrently_, race_, webSockets)

mkYesodDispatch "App" resourcesApp

data HomeData = HomeData
  { hNiceName :: Text,
    hPath :: Path Abs Dir,
    hLastModified :: UTCTime,
    hLastWatched :: UTCTime,
    hVideoFileCount :: Int,
    hPlayedVideoFileCount :: Int
  }

data NamedLink = NamedLink
  { linkName :: Text,
    linkImage :: Route App,
    linkUrl :: Text
  }

data HomeSection
  = LocalVideos Text [HomeData]
  | ExternalLinks Text [NamedLink]

getHomeR :: Handler Html
getHomeR = do
  -- This can be a websocket request, so do that
  tvStateTVar <- getsYesod appTVState
  webSockets $ race_ actionsWebSocket (tvStateWebSocket tvStateTVar)

  homeDir <- liftIO getVideoDirPath -- For now we just use the hardcoded path as home, but the plan is to support multiple root folders
  let epoch = posixSecondsToUTCTime 0
  homeData' <-
    logDuration "Queried DB for home data" $
      map unSingle5
        <$> runDB
          [sqlQQ|
            SELECT
              d.@{DirectoryPath},
              max(v.@{VideoFileAdded}),
              COALESCE(max(v.@{VideoFileWatched}), #{epoch}),
              count(v.@{VideoFilePath}),
              SUM(IIF(v.@{VideoFileWatched} IS NOT NULL, 1, 0))
            FROM ^{Directory} d
            LEFT JOIN ^{VideoFile} v
              ON v.@{VideoFilePath} GLOB d.@{DirectoryPath} || '*'
            WHERE 
              -- This checks that it's a sub-directory
              -- SQLite has some GLOB optimisations, so this is fast
              d.@{DirectoryPath} GLOB #{homeDir} || '*'
              -- This makes sure it's a direct child
              AND instr(rtrim(substr(d.@{DirectoryPath}, length(#{homeDir})+1), '/'), '/') = 0
              -- And this removed the homeDir itself
              AND d.@{DirectoryPath} <> #{homeDir}
            GROUP BY
              d.@{DirectoryPath}
          |]
  let toHomeData ::
        ( Path Abs Dir,
          UTCTime,
          UTCTime,
          Int,
          Int
        ) ->
        HomeData
      toHomeData d = uncurry5 (HomeData $ niceDirNameT $ fst5 d) d

  let homeData = map toHomeData homeData'
  let mkRandom =
        -- When in dev we auto-reload the page every second or so,
        -- so we want the same random shuffle every time, otherwise the page
        -- keeps changing which is annoying.
        if isDevelopment then pure (mkStdGen 2) else initStdGen
  randomGenerator <- mkRandom
  let isUnwatched d = d.hPlayedVideoFileCount < d.hVideoFileCount
      unwatched = filter isUnwatched homeData
      recentlyAdded d = Down d.hLastModified
      recentlyWatched d = d.hLastWatched

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

getDirectoryR :: Path Abs Dir -> Handler Html
getDirectoryR absPath = do
  (dirs :: [Path Abs Dir], files :: [VideoFile]) <- logDuration "Get child dirs and files" $ runDB $ do
    ds <-
      [sqlQQ|
        SELECT @{DirectoryPath}
        FROM ^{Directory}
        WHERE
          -- This checks that it's a sub-directory
          @{DirectoryPath} GLOB #{absPath} || '*'
          -- This makes sure it's a direct child
          AND instr(rtrim(substr(@{DirectoryPath}, length(#{absPath})+1), '/'), '/') = 0
          -- And this removed the homeDir itself
          AND @{DirectoryPath} <> #{absPath}
      |]
    fs <-
      [sqlQQ|
        SELECT ??
        FROM ^{VideoFile}
        WHERE
          -- This checks that it's a sub-file
          @{VideoFilePath} GLOB #{absPath} || '*'
          -- This makes sure it's a direct child
          AND instr(substr(@{VideoFilePath}, length(#{absPath})+1), '/') = 0
      |]
    pure (map unSingle ds, map entityVal fs)

  let watchedClass :: Maybe UTCTime -> Html
      watchedClass = \case
        Just _ -> "watched"
        Nothing -> "unwatched"

  let title = toHtml $ niceDirNameT absPath
  defaultLayout title $(widgetFile "directory")

getRemoteR :: Handler Html
getRemoteR = do
  defaultLayout "Remote" $(widgetFile "remote")

getImageR :: Path Abs Dir -> Handler Html
getImageR absPath = do
  (image :: [(Path Rel File, BS.ByteString)]) <-
    logDuration "Get image" $
      runDB $
        map unSingle2
          <$> [sqlQQ|
                SELECT @{DirectoryImageName}, @{DirectoryImage}
                FROM ^{Directory}
                WHERE
                    @{DirectoryImageName} IS NOT NULL
                AND @{DirectoryImage} IS NOT NULL
                AND (
                  -- Any image that is in the given path, or any of it's parents
                  #{absPath} GLOB @{DirectoryPath} || '*'
                  -- Or any image in a child folder
                  OR @{DirectoryPath} GLOB #{absPath} || '*'
                )
                LIMIT 1
              |]
  case image of
    [] -> notFound
    (imgName, imgBytes) : _ -> sendResponse (getImageContentType imgName, toContent imgBytes)

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
      logDuration "Migration" $ runDBWithConn connPool $ runMigration migrateAll

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
