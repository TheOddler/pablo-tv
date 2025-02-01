{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (actionsWebSocket, mkInputDevice, performAction)
import Control.Monad (filterM, when)
import Data.Aeson (Result (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.List (foldl')
import Data.List.Extra (notNull, replace)
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Lazy.Builder (fromText)
import Data.Time (diffUTCTime, getCurrentTime)
import Directory
  ( DirectoryInfo (..),
    DirectoryKind (..),
    DirectoryRaw (..),
    niceDirNameT,
    niceFileNameT,
    readDirectoryInfoRec,
    readDirectoryRaw,
    updateAllDirectoryInfos,
  )
import Evdev.Uinput (Device)
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
    fromAbsFile,
    mkRelDir,
    parent,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO (doesDirExist, doesFileExist, getHomeDir, listDir)
import System.Environment (getEnv)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (callProcess)
import System.Random (initStdGen, mkStdGen)
import TVDB (TVDBToken (..))
import TVState (TVState (..), startingTVState, tvStateWebSocket)
import Text.Hamlet (hamletFile)
import Text.Julius (RawJavascript (..))
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
import Watched (WatchedInfoAgg (..), readWatchedInfoAgg)
import Yesod hiding (defaultLayout, replace)
import Yesod qualified
import Yesod.EmbeddedStatic
import Yesod.WebSockets (race_, webSockets)

data App = App
  { appPort :: Int,
    appTVDBToken :: TVDBToken,
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

type DirData = (DirName, DirSegments, WatchedCount, TotalCount)

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
  webSockets $
    race_
      (actionsWebSocket inputDevice tvStateTVar)
      (tvStateWebSocket tvStateTVar)

  -- Get proper data (we got this async from the state)
  tvState <- liftIO $ readTVarIO tvStateTVar
  let videoData = tvVideoData tvState
  let mkRandom =
        -- When in dev we auto-reload the page every second or so,
        -- so we want the same random shuffle every time, otherwise the page
        -- keeps changing which is annoying.
        if isDevelopment then pure (mkStdGen 2) else initStdGen
  randomGenerator <- mkRandom
  let videosSortedWith ::
        (Ord a) =>
        ((Path Abs Dir, DirectoryInfo, WatchedInfoAgg) -> a) ->
        [DirData]
      videosSortedWith f = mkDirData <$> sortWith f videoData

  let sections =
        [ LocalVideos "Recently Added" $
            videosSortedWith (\(_, _, i) -> -i.watchedInfoLastModified),
          LocalVideos "Unwatched" $
            let isUnwatched (_, _, i) = i.watchedInfoPlayedVideoFileCount < i.watchedInfoVideoFileCount
                unwatched = filter isUnwatched videoData
             in if notNull unwatched
                  then mkDirData <$> shuffle unwatched randomGenerator
                  else [],
          ExternalLinks
            "External Links"
            [ ("YouTube", StaticR static_images_youtube_png, "https://www.youtube.com/feed/subscriptions"),
              ("Netflix", StaticR static_images_netflix_png, "https://www.netflix.com"),
              ("Apple TV+", StaticR static_images_apple_tv_plus_png, "https://tv.apple.com")
            ],
          LocalVideos "Random" $
            mkDirData
              <$> shuffle videoData randomGenerator
        ]

  defaultLayout "Home" $(widgetFile "home")
  where
    fileNameToSegments :: Path Rel a -> [Text]
    fileNameToSegments f = [T.pack $ toFilePath f]

    mkDirData ::
      (Path Abs Dir, DirectoryInfo, WatchedInfoAgg) -> DirData
    mkDirData (path, info, watchedInfo) =
      let segments = fileNameToSegments $ dirname path
       in ( directoryInfoTitle info,
            segments,
            watchedInfo.watchedInfoPlayedVideoFileCount,
            watchedInfo.watchedInfoVideoFileCount
          )

postHomeR :: Handler ()
postHomeR =
  parseCheckJsonBody >>= \case
    Error s -> do
      liftIO $ putStrLn $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Success action -> do
      inputDevice <- getsYesod appInputDevice
      tvStateTVar <- getsYesod appTVState
      liftIO $ performAction inputDevice tvStateTVar action

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
  let filesWithNames = map (\f -> (niceFileNameT f, f)) dirRaw.directoryVideoFiles
      dirsWithNames = map (\d -> (niceDirNameT d, d)) dirRaw.directoryDirectories

  let mkSegments :: Path Rel x -> [Text]
      mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

      absPathJS :: RawJavascript
      absPathJS = RawJavascript . fromText . T.pack $ toFilePath absPath

      mkAbsFilePath :: Path Rel File -> String
      mkAbsFilePath filename = replace "'" "\\'" $ fromAbsFile $ absPath </> filename

  let title = toHtml $ (directoryInfoTitle <$> mInfo) `orElse` "Videos"
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

main :: IO ()
main = do
  -- To get this token for now you can use your apiKey here https://thetvdb.github.io/v4-api/#/Login/post_login
  tvdbToken <- TVDBToken . BS.pack <$> getEnv "TVDB_TOKEN"

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

  let dataThread =
        asyncOnTrigger videoDataRefreshTrigger $ do
          startTime <- getCurrentTime
          infosWithPath <- updateAllDirectoryInfos tvdbToken
          let addFSInfo (path, info) = do
                infoFS <- readWatchedInfoAgg path
                pure (path, info, infoFS)
          infos <- mapM addFSInfo infosWithPath
          atomically $ do
            state <- readTVar tvState
            writeTVar tvState state {tvVideoData = infos}
          endTime <- getCurrentTime
          putStrLn $ "Refreshed video data in " ++ show (diffUTCTime endTime startTime)

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
  race_ appThread dataThread

  putStrLn "Server quite."
