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
import Data.Text.Lazy.Builder (fromText)
import Directory
  ( DirectoryInfo (..),
    DirectoryInfoFS (..),
    DirectoryKind (..),
    DirectoryRaw (..),
    getVideoDirPath,
    niceDirNameT,
    niceFileNameT,
    readDirectoryInfoFS,
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
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
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
import System.Random.Shuffle (shuffle')
import TVDB (TVDBToken (..))
import Text.Hamlet (hamletFile)
import Text.Julius (RawJavascript (..))
import Util
  ( asyncOnTrigger,
    networkInterfacesShortList,
    onChanges,
    removeLast,
    toUrl,
    unsnoc,
    widgetFile,
  )
import Yesod hiding (defaultLayout, replace)
import Yesod qualified
import Yesod.EmbeddedStatic
import Yesod.WebSockets (race_, sendTextData, webSockets)

data App = App
  { appPort :: Int,
    appTVDBToken :: TVDBToken,
    appInputDevice :: Device,
    appTVState :: TVar TVState,
    appGetStatic :: EmbeddedStatic,
    appVideoDataRefreshTrigger :: MVar ()
  }

data TVState = TVState
  { tvPage :: Route App,
    tvVideoData :: [(Path Abs Dir, DirectoryInfo, DirectoryInfoFS)]
  }

instance (Eq (Route App)) => Eq TVState where
  TVState a b == TVState a2 b2 = a == a2 && b == b2

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
    embedFile "static/fontawesome/webfonts/fa-solid-900.woff2"
  ]

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
/tv TVR GET
/ips AllIPsR GET
/trackpad TrackpadR GET
/pointer MousePointerR GET
/keyboard KeyboardR GET
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

      addScript $ StaticR static_reconnecting_websocket_js
      addStylesheet $ StaticR static_fontawesome_css_all_min_css
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

getHomeR :: Handler Html
getHomeR = do
  inputDevice <- getsYesod appInputDevice
  webSockets $ actionsWebSocket inputDevice
  defaultLayout "Home" $(widgetFile "home")

postHomeR :: Handler ()
postHomeR =
  parseCheckJsonBody >>= \case
    Error s -> do
      liftIO $ putStrLn $ "Failed parsing action: " ++ s
      invalidArgs [Text.pack s]
    Success action -> do
      inputDevice <- getsYesod appInputDevice
      liftIO $ performAction inputDevice action

getTrackpadR :: Handler Html
getTrackpadR =
  defaultLayout "Trackpad" $(widgetFile "trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  defaultLayout "Pointer" $(widgetFile "mouse-pointer")

getKeyboardR :: Handler Html
getKeyboardR =
  defaultLayout "Keyboard" $(widgetFile "keyboard")

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
  isTv <- isTvRequest
  absPath <-
    parseSegments segments >>= \case
      Just (p, Nothing) -> pure p
      _ ->
        let home = if isTv then TVR else HomeR
         in redirect $ maybe home DirectoryR $ removeLast segments

  mPathAndInfo <- liftIO $ readDirectoryInfoRec absPath
  let mInfo = snd <$> mPathAndInfo
  dirRaw <- liftIO $ readDirectoryRaw absPath
  let filesWithNames = map (\f -> (niceFileNameT f, f)) dirRaw.directoryVideoFiles
      dirsWithNames = map (\d -> (niceDirNameT d, d)) dirRaw.directoryDirectories

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ do
    state <- readTVar tvStateTVar
    writeTVar tvStateTVar state {tvPage = DirectoryR segments}

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

getTVR :: Handler Html
getTVR = do
  -- TV has it's own web socket to not interfere with the rest of the app
  tvStateTVar <- getsYesod appTVState
  webSockets $ onChanges tvStateTVar $ \tvState -> do
    toUrl (tvPage tvState) >>= sendTextData

  -- Do the non-websocket stuff
  networkInterfaces <- networkInterfacesShortList <$> liftIO getNetworkInterfaces
  port <- getsYesod appPort

  -- See if there are any files in the root, ideally there shouldn't be because
  -- we won't have info for them. But I do want to support it, so we'll still
  -- show them.
  videoDir <- liftIO getVideoDirPath
  dirRaw <- liftIO $ readDirectoryRaw videoDir
  let files = map (\f -> (niceFileNameT f, fileNameToSegments f)) dirRaw.directoryVideoFiles

  -- Get proper data (we got this async from the state)
  tvState <- liftIO $ readTVarIO tvStateTVar
  let videoData = tvVideoData tvState

  -- Different orderings of the video data that we want to use
  randomGenerator <-
    -- When in dev we auto-reload the page every second or so,
    -- so we want the same random shuffle every time, otherwise the page
    -- keeps changing which is annoying.
    if isDevelopment then pure (mkStdGen 2) else initStdGen
  let videosRandom = nameAndSegments <$> shuffle' videoData (length videoData) randomGenerator
  let videosSortedWith :: (Ord a) => ((Path Abs Dir, DirectoryInfo, DirectoryInfoFS) -> a) -> [(Text, [Text])]
      videosSortedWith f = nameAndSegments <$> sortWith f videoData
  let videosAlphabetical = videosSortedWith (\(_, i, _) -> i.directoryInfoTitle)
  let videosNewest = videosSortedWith (\(_, _, i) -> -i.directoryDataLastModified)
  let videosUnseen = nameAndSegments <$> filter (\(_, _, i) -> i.directoryDataPlayedVideoFileCount < i.directoryDataVideoFileCount) videoData

  defaultLayout "TV" $(widgetFile "tv")
  where
    ipV4OrV6WithPort port i =
      ( if ipv4 i == IPv4 0
          then show $ ipv6 i
          else show $ ipv4 i
      )
        ++ ":"
        ++ show port

    fileNameToSegments :: Path Rel a -> [Text]
    fileNameToSegments f = [T.pack $ toFilePath f]

    nameAndSegments :: (Path Abs Dir, DirectoryInfo, DirectoryInfoFS) -> (Text, [Text])
    nameAndSegments (path, info, _) =
      let segments = fileNameToSegments $ dirname path
       in (directoryInfoTitle info, segments)

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces <- liftIO getNetworkInterfaces
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
  tvState <- newTVarIO $ TVState HomeR []
  videoDataRefreshTrigger <- newMVar ()

  let port = 8080
  let (tvPath, _params) = renderRoute TVR
  let url = "http://localhost:" ++ show port ++ "/" ++ unpack (intercalate "/" tvPath)
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    callProcess "xdg-open" [url]

  let dataThread =
        asyncOnTrigger videoDataRefreshTrigger $ do
          infosWithPath <- updateAllDirectoryInfos tvdbToken
          let addFSInfo (path, info) = do
                infoFS <- readDirectoryInfoFS path
                pure (path, info, infoFS)
          infos <- mapM addFSInfo infosWithPath
          atomically $ do
            state <- readTVar tvState
            writeTVar tvState state {tvVideoData = infos}

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

  race_ appThread dataThread

  putStrLn "Server quite."
