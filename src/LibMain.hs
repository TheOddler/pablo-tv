{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (actionsWebSocket, mkInputDevice)
import Control.Monad (filterM, join, when)
import Data.Char (toLower)
import Data.List (foldl', isSuffixOf)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Evdev.Uinput (Device)
import Files (DirectoryInfo (..), EpisodeInfo (..), EpisodeNumber (..), MovieInfo (..), parseDirectory)
import GHC.Conc (TVar, atomically, newTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM, listToMaybe)
import IsDevelopment (isDevelopment)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path (Abs, Dir, File, Path, Rel, fileExtension, fromAbsDir, fromAbsFile, parent, parseAbsDir, parseRelFile, toFilePath, (</>))
import System.Directory (doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath (combine, dropTrailingPathSeparator)
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Text.Julius (Javascript, jsFile)
import Util (networkInterfacesShortList, onChanges, toUrl, widgetFile)
import Yesod hiding (defaultLayout)
import Yesod qualified
import Yesod.WebSockets (sendTextData, webSockets)

data App = App
  { appPort :: Int,
    appInputDevice :: Device,
    appTVState :: TVar TVState
  }

newtype TVState = TVState
  { tvPage :: Route App
  }

instance (Eq (Route App)) => Eq TVState where
  TVState a == TVState b = a == b

mkYesod
  "App"
  [parseRoutes|
-- Routes for the mobile app
/ MobileHomeR GET
/trackpad TrackpadR GET
/pointer MousePointerR GET
/keyboard KeyboardR GET
/input InputR GET
/dir/+Texts DirectoryR GET

-- Routes for the 
/tv TVHomeR GET
/ips AllIPsR GET

-- Other
/image/+Texts ImageR GET
/reconnecting-websocket.js ReconnectingWebSocketJSR GET
|]

instance Yesod App where
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"
      addScript ReconnectingWebSocketJSR
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
  webSockets $ actionsWebSocket inputDevice
  mobileLayout $(widgetFile "mobile/home")

getTrackpadR :: Handler Html
getTrackpadR =
  mobileLayout $(widgetFile "mobile/trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  mobileLayout $(widgetFile "mobile/mouse-pointer")

getKeyboardR :: Handler Html
getKeyboardR =
  mobileLayout $(widgetFile "mobile/keyboard")

getPathFromSegments :: [Text] -> Handler (Path Abs Dir)
getPathFromSegments segments = do
  home <- liftIO getHomeDirectory
  let currentPath = combine home $ foldl' combine "Videos" (unpack <$> segments)
  parseAbsDir currentPath

getDirectoryR :: [Text] -> Handler Html
getDirectoryR segments = do
  absPath <- getPathFromSegments segments
  (info, directories) <- liftIO $ parseDirectory absPath

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ DirectoryR segments

  mobileLayout $(widgetFile "mobile/directory")
  where
    showEpisode :: EpisodeInfo -> String
    showEpisode e =
      ( if e.episodeSpecial
          then " Special "
          else "Season " ++ show e.episodeSeason ++ " Episode "
      )
        ++ ( case e.episodeNumber of
               EpisodeNumber n -> show n
               EpisodeNumberDouble n m -> show n ++ " and " ++ show m
           )

    showPath :: Path Rel x -> String
    showPath = dropTrailingPathSeparator . toFilePath

    mkSegments :: Path Rel x -> [Text]
    mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

getImageR :: [Text] -> Handler Html
getImageR segments = do
  dir <- getPathFromSegments segments
  mImg <- firstJustsM $ tryGetImage <$> take 3 (iterate parent dir)
  case mImg of
    Just (contentType, path) ->
      sendFile contentType $ fromAbsFile path
    Nothing ->
      notFound
  where
    tryGetImage :: Path Abs Dir -> Handler (Maybe (ContentType, Path Abs File))
    tryGetImage dir = do
      fileAndDirNames <- liftIO $ listDirectory $ fromAbsDir dir
      let hasImageExt :: FilePath -> Bool
          hasImageExt file = any (`isSuffixOf` file) [".jpg", ".jpeg", ".png", ".gif"]
          isImage :: FilePath -> IO Bool
          isImage file =
            if not $ hasImageExt file
              then pure False
              else doesFileExist $ combine (fromAbsDir dir) file
      mImageFile <- liftIO $ join . mapM parseRelFile . listToMaybe <$> filterM isImage fileAndDirNames
      case mImageFile of
        Nothing -> pure Nothing
        Just imageFile -> do
          let absPath = dir </> imageFile
          ext <- fileExtension imageFile
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

getReconnectingWebSocketJSR :: Handler Javascript
getReconnectingWebSocketJSR =
  withUrlRenderer $(jsFile "templates/reconnecting-websocket.js")

main :: IO ()
main = do
  inputDevice <- mkInputDevice
  tvState <- newTVarIO $ TVState MobileHomeR

  let port = 8080
  let url = "http://localhost:" ++ show port `combine` "tv"
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    let (path, _params) = renderRoute TVHomeR
    callProcess "xdg-open" [url ++ unpack (intercalate "/" path)]

  app <-
    toWaiAppPlain
      App
        { appPort = port,
          appInputDevice = inputDevice,
          appTVState = tvState
        }
  run port $ defaultMiddlewaresNoLogging app
