{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (actionsWebSocket, mkInputDevice)
import Control.Monad (filterM, when)
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Data.Text qualified as T
import Evdev.Uinput (Device)
import Files (DirectoryInfo (..), DirectoryKind (..), niceFileNameT, parseDirectory)
import GHC.Conc (TVar, atomically, newTVarIO, writeTVar)
import GHC.Data.Maybe (firstJustsM, listToMaybe)
import IsDevelopment (isDevelopment)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import Network.Wai.Handler.Warp (run)
import Path (Abs, Dir, File, Path, Rel, fileExtension, fromAbsFile, mkRelDir, parent, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (doesDirExist, doesFileExist, getHomeDir, listDir)
import System.Environment (getEnv)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Text.Julius (Javascript, jsFile)
import Util (networkInterfacesShortList, onChanges, toUrl, widgetFile)
import Yesod hiding (defaultLayout)
import Yesod qualified
import Yesod.WebSockets (sendTextData, webSockets)

data App = App
  { appPort :: Int,
    appTVDBToken :: BS.ByteString,
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
/file/+Texts FileR GET

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

-- | Not yet available in the base I use, but should be replaceable in a later version
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

removeLast :: [a] -> [a]
removeLast = reverse . drop 1 . reverse

getDirectoryR :: [Text] -> Handler Html
getDirectoryR segments = do
  absPath <-
    parseSegments segments >>= \case
      Just (p, Nothing) -> pure p
      _ -> redirect $ DirectoryR $ removeLast segments
  tvdbToken <- getsYesod appTVDBToken
  (mInfo, filesWithNames, dirsWithNames) <- liftIO $ parseDirectory tvdbToken absPath

  -- If there's one file, this is a movie, and there are no other sub-directories, redirect to the file page
  case filesWithNames of
    [(_niceBame, name)]
      | (directoryInfoKind <$> mInfo) == Just DirectoryKindMovie && null dirsWithNames ->
          redirect $ FileR $ mkSegments name
    _ -> pure ()

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ DirectoryR segments

  mobileLayout $ do
    $(widgetFile "mobile/directory-and-file")
    $(widgetFile "mobile/directory")
  where
    mkSegments :: Path Rel x -> [Text]
    mkSegments d = segments ++ [T.pack $ dropTrailingPathSeparator $ toFilePath d]

getFileR :: [Text] -> Handler Html
getFileR segments = do
  (_dir, fileName) <-
    parseSegments segments >>= \case
      Just (dir, Just path) -> pure (dir, path)
      _ -> redirect $ DirectoryR $ removeLast segments

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ FileR segments

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

getReconnectingWebSocketJSR :: Handler Javascript
getReconnectingWebSocketJSR =
  withUrlRenderer $(jsFile "templates/reconnecting-websocket.js")

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

  app <-
    toWaiAppPlain
      App
        { appPort = port,
          appTVDBToken = BS.pack tvdbToken,
          appInputDevice = inputDevice,
          appTVState = tvState
        }
  run port $ defaultMiddlewaresNoLogging app
