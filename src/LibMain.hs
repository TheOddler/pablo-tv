{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (Action, actionsWebSocket, mkInputDevice, performAction)
import Control.Monad (filterM, when)
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Data.Text.Lazy.Builder (fromText)
import Directory (DirectoryInfo (..), DirectoryKind (..), niceFileNameT, parseDirectory)
import Evdev.Uinput (Device)
import GHC.Conc (TVar, atomically, newTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM, listToMaybe)
import IsDevelopment (isDevelopment)
import MPV (MPV, withMPV)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path (Abs, Dir, File, Path, Rel, fileExtension, fromAbsFile, mkRelDir, parent, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (doesDirExist, doesFileExist, getHomeDir, listDir)
import System.Environment (getEnv)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Text.Julius (RawJavascript (..))
import Util (networkInterfacesShortList, onChanges, removeLast, toUrl, unsnoc, widgetFile)
import Yesod hiding (defaultLayout)
import Yesod qualified
import Yesod.EmbeddedStatic
import Yesod.WebSockets (sendTextData, webSockets)

data App = App
  { appPort :: Int,
    appTVDBToken :: BS.ByteString,
    appInputDevice :: Device,
    appTVState :: TVar TVState,
    appMPV :: MPV,
    appGetStatic :: EmbeddedStatic
  }

newtype TVState = TVState
  { tvPage :: Route App
  }

instance (Eq (Route App)) => Eq TVState where
  TVState a == TVState b = a == b

mkEmbeddedStatic
  False
  "embeddedStatic"
  [ embedFile "static/reconnecting-websocket.js",
    embedFile "static/fontawesome/all.min.css"
  ]

mkYesod
  "App"
  [parseRoutes|
-- Routes for the mobile app
/ MobileHomeR GET POST
/trackpad TrackpadR GET
/pointer MousePointerR GET
/keyboard KeyboardR GET
/input InputR GET
/dir/+Texts DirectoryR GET
/file/+Texts FileR GET

-- Routes for the 
/tv TVHomeR GET
/ips AllIPsR GET

-- Other
/image/+Texts ImageR GET
/static StaticR EmbeddedStatic appGetStatic
|]

instance Yesod App where
  addStaticContent = embedStaticContent appGetStatic StaticR Right
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"
      addScript $ StaticR static_reconnecting_websocket_js
      addStylesheet $ StaticR static_fontawesome_all_min_css
      $(widgetFile "shared-default")
    withUrlRenderer $
      $(hamletFile "templates/shared-page-wrapper.hamlet")

mobileLayout :: Widget -> Handler Html
mobileLayout widget = Yesod.defaultLayout $ do
  when isDevelopment $ do
    addScriptRemote "//cdn.jsdelivr.net/npm/eruda" -- Console for mobile
    toWidgetBody
      [julius|
        window.onload = function() {
          eruda.init();
        };
      |]
  $(widgetFile "mobile/default")

getMobileHomeR :: Handler Html
getMobileHomeR = do
  inputDevice <- getsYesod appInputDevice
  mpv <- getsYesod appMPV
  webSockets $ actionsWebSocket inputDevice mpv
  mobileLayout $(widgetFile "mobile/home")

postMobileHomeR :: Handler ()
postMobileHomeR = do
  (action :: Action) <- requireCheckJsonBody
  inputDevice <- getsYesod appInputDevice
  mpv <- getsYesod appMPV
  liftIO $ performAction inputDevice mpv action

getTrackpadR :: Handler Html
getTrackpadR =
  mobileLayout $(widgetFile "mobile/trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  mobileLayout $(widgetFile "mobile/mouse-pointer")

getKeyboardR :: Handler Html
getKeyboardR =
  mobileLayout $(widgetFile "mobile/keyboard")

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
      _ -> redirect $ DirectoryR $ removeLast segments
  tvdbToken <- getsYesod appTVDBToken
  (mInfo, filesWithNames, dirsWithNames) <- liftIO $ parseDirectory tvdbToken absPath

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ DirectoryR segments

  let mkSegments :: Path Rel x -> [Text]
      mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

      mkAbsFilePath :: Path Rel File -> String
      mkAbsFilePath filename = fromAbsFile $ absPath </> filename

  mobileLayout $ do
    $(widgetFile "mobile/directory-and-file")
    $(widgetFile "mobile/directory")

getFileR :: [Text] -> Handler Html
getFileR segments = do
  (dir, fileName) <-
    parseSegments segments >>= \case
      Just (dir, Just path) -> pure (dir, path)
      _ -> redirect $ DirectoryR $ removeLast segments

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ FileR segments

  let absFilePath = dir </> fileName
  let cleanAbsFilePath = RawJavascript . fromText . T.pack $ fromAbsFile absFilePath

  mobileLayout $ do
    $(widgetFile "mobile/directory-and-file")
    $(widgetFile "mobile/file")

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
getInputR =
  mobileLayout $(widgetFile "mobile/input")

tvLayout :: Widget -> Handler Html
tvLayout widget = Yesod.defaultLayout $(widgetFile "tv/default")

getTVHomeR :: Handler Html
getTVHomeR = do
  -- TV has it's own web socket to not interfere with the mobile app
  tvStateTVar <- getsYesod appTVState
  webSockets $ onChanges tvStateTVar $ \tvState -> do
    toUrl (tvPage tvState) >>= sendTextData

  networkInterfaces <- networkInterfacesShortList <$> liftIO getNetworkInterfaces
  port <- getsYesod appPort
  tvLayout $(widgetFile "tv/home")
  where
    ipV4OrV6WithPort port i =
      ( if ipv4 i == IPv4 0
          then show $ ipv6 i
          else show $ ipv4 i
      )
        ++ ":"
        ++ show port

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces <- liftIO getNetworkInterfaces
  port <- getsYesod appPort
  tvLayout $(widgetFile "tv/ips")
  where
    hideZero :: (Show a, Eq a, Bounded a) => a -> String
    hideZero a =
      if a == minBound
        then ""
        else show a

main :: IO ()
main = do
  -- To get this token for now you can use your apiKey here https://thetvdb.github.io/v4-api/#/Login/post_login
  tvdbToken <- getEnv "TVDB_TOKEN"

  inputDevice <- mkInputDevice
  tvState <- newTVarIO $ TVState MobileHomeR

  let port = 8080
  let url = "http://localhost:" ++ show port ++ "/tv"
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    let (path, _params) = renderRoute TVHomeR
    callProcess "xdg-open" [url ++ unpack (intercalate "/" path)]

  withMPV $ \mpv -> do
    app <-
      toWaiAppPlain
        App
          { appPort = port,
            appTVDBToken = BS.pack tvdbToken,
            appInputDevice = inputDevice,
            appTVState = tvState,
            appMPV = mpv,
            appGetStatic = embeddedStatic
          }
    run port $ defaultMiddlewaresNoLogging app

  putStrLn "Server quite."
