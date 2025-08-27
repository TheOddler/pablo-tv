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
import Control.Monad (filterM, when)
import Control.Monad.Logger (runStderrLoggingT)
import DB (Directory (..), EntityField (..), VideoFile (..), migrateAll, runDBWithConn)
import Data.Aeson (Result (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace, toLower)
import Data.List (foldl')
import Data.List.Extra (notNull)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql.Raw.QQ (sqlQQ)
import Database.Persist.Sqlite (runMigration, withSqlitePool)
import DirectoryNew (getVideoDirPath, niceDirNameT, updateData)
import Foundation (App (..), Handler, Route (..), defaultLayout, embeddedStatic, resourcesApp, static_images_apple_tv_plus_png, static_images_netflix_png, static_images_youtube_png)
import GHC.Conc (newTVarIO)
import GHC.Data.Maybe (firstJustsM, listToMaybe)
import GHC.MVar (newMVar)
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
    fileExtension,
    fromAbsFile,
    isProperPrefixOf,
    mkRelDir,
    parent,
    parseRelDir,
    parseRelFile,
    (</>),
  )
import Path.IO (doesDirExist, doesFileExist, getHomeDir, listDir)
import Playerctl (Action (..), onFilePlayStarted)
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBToken (..))
import TVState (startingTVState, tvStateWebSocket)
import Util
  ( fst5,
    logDuration,
    networkInterfaceWorthiness,
    safeMaxUTCTime,
    shuffle,
    unSingle5,
    uncurry5,
    unsnoc,
    widgetFile,
  )
import Yesod hiding (defaultLayout, replace)
import Yesod.WebSockets (race_, webSockets)

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
    Error s -> do
      liftIO $ putStrLn $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Success action -> do
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

data DirData = DirData
  { dirNiceName :: Text,
    dirPath :: Path Abs Dir,
    dirLastModified :: UTCTime,
    dirLastWatched :: UTCTime,
    dirVideoFileCount :: Int,
    dirPlayedVideoFileCount :: Int,
    dirChildFiles :: [Path Abs File],
    dirChildDirs :: [Path Abs Dir]
  }

getDirData :: [Directory] -> [VideoFile] -> Path Abs Dir -> DirData
getDirData allDirs allFiles dir =
  DirData
    { dirNiceName = niceDirNameT dir,
      dirPath = dir,
      dirLastModified =
        safeMaxUTCTime $ map videoFileAdded allDescendantFiles,
      dirLastWatched =
        safeMaxUTCTime $ mapMaybe videoFileWatched allDescendantFiles,
      dirVideoFileCount =
        length allDescendantFiles,
      dirPlayedVideoFileCount =
        length $ filter (isJust . videoFileWatched) allDescendantFiles,
      dirChildFiles = childFiles,
      dirChildDirs = childDirs
    }
  where
    allDescendantFiles =
      filter (\f -> dir `isProperPrefixOf` videoFilePath f) allFiles
    childFiles =
      filter ((dir ==) . parent) $
        map videoFilePath allDescendantFiles
    childDirs =
      filter ((dir ==) . parent) $
        map directoryPath allDirs

getDirectoryR :: Path Abs Dir -> Handler Html
getDirectoryR absPath = do
  liftIO $ putStrLn $ "Path: " ++ show absPath

  (allDirs' :: [Entity Directory], allFiles' :: [Entity VideoFile]) <- logDuration "readWholeDB" $ runDB $ do
    (,) <$> selectList [] [] <*> selectList [] []
  let allDirs = map entityVal allDirs'
  let allFiles = map entityVal allFiles'
  let dirData = getDirData allDirs allFiles absPath

  notFound

-- mPathAndInfo <- liftIO $ readDirectoryInfoRec absPath
-- let mInfo = snd <$> mPathAndInfo
-- dirRaw <- liftIO $ readDirectoryRaw absPath

-- let files :: [(Path Abs File, Text)]
--     files = map (\f -> (absPath </> f, niceFileNameT f)) dirRaw.directoryVideoFiles

-- -- Get the dirs info. Depending on where we are we do different things:
-- -- \* At the root (segments is empty): We have the required data cached in the tv state. We do this because this folder is expected to have many subfolders.
-- -- \* In a sub-dir: We read the required stuff from disk. We do this so we always have to most up-to-date data, and we don't expect too much stuff in here, so should be fine to read anew.
-- dirs :: [(Path Rel Dir, Text, Maybe (Int, Int))] <- case segments of
--   [] -> do
--     -- tvState <- getsYesod appTVState >>= liftIO . readTVarIO
--     let tempVideoData = mempty
--     videoDirPath <- liftIO getVideoDirPath
--     topLevelDirs <- liftIO $ getTopLevelDirs videoDirPath
--     let getDir :: Directory -> (Path Rel Dir, Text, Maybe (Int, Int))
--         getDir dir =
--           let dirName = dirname $ topLevelToAbsDir dir
--               dirInfo = Map.lookup dir tempVideoData
--            in ( dirName,
--                 maybe (niceDirNameT dirName) (directoryInfoTitle . fst) dirInfo,
--                 case snd <$> dirInfo of
--                   Nothing -> Nothing
--                   Just w ->
--                     Just
--                       ( watchedInfoPlayedVideoFileCount w,
--                         watchedInfoVideoFileCount w
--                       )
--               )
--     pure $ getDir <$> topLevelDirs
--   _ -> do
--     let getDir :: Path Rel Dir -> Handler (Path Rel Dir, Text, Maybe (Int, Int))
--         getDir dirName = do
--           watched <- liftIO $ readWatchedInfoAgg $ absPath </> dirName
--           pure
--             ( dirName,
--               niceDirNameT dirName,
--               Just
--                 ( watched.watchedInfoPlayedVideoFileCount,
--                   watched.watchedInfoVideoFileCount
--                 )
--             )
--     mapM getDir dirRaw.directoryDirectories

-- let mkSegments :: Path Rel x -> [Text]
--     mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

--     mkAbsFilePath :: Path Abs File -> String
--     mkAbsFilePath filePath = replace "'" "\\'" $ fromAbsFile filePath

-- watchedFiles <- liftIO $ readWatchedInfo absPath
-- let watchedClassFile :: Path Abs File -> String
--     watchedClassFile filePath =
--       if hasBeenWatched watchedFiles filePath
--         then "watched"
--         else "unwatched"

--     watchedClassDir :: Maybe (Int, Int) -> String
--     watchedClassDir Nothing = "white"
--     watchedClassDir (Just (watchedCount, totalCount)) =
--       if watchedCount < totalCount
--         then "unwatched"
--         else "watched"

-- let title = toHtml $ (directoryInfoTitle <$> mInfo) `orElse` "Videos"
-- let showRefreshButton = null segments
-- defaultLayout title $(widgetFile "directory")

getRemoteR :: Handler Html
getRemoteR = do
  defaultLayout "Remote" $(widgetFile "remote")

getImageR :: Path Abs Dir -> Handler Html
getImageR dir = do
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

main :: IO ()
main = do
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

  let openConnectionCount = 10
  runStderrLoggingT $
    withSqlitePool "pablo-tv-data.db3" openConnectionCount $ \connPool -> liftIO $ do
      -- Migrate DB
      runDBWithConn connPool $ runMigration migrateAll

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
                putStrLn $ "Playing file: " ++ show path
                performActionIO app $ ActionMarkAsWatched $ File path

      putStrLn "Starting race..."
      -- concurrently_ dataThread $
      race_ appThread watchedThread

  putStrLn "Server quite."
