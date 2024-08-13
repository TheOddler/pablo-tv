{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (actionsWebSocket, mkInputDevice)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Text (Text, intercalate, pack, unpack)
import Evdev.Uinput (Device)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import GHC.Utils.Monad (partitionM)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import System.Directory (doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath (combine, (</>))
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Text.Julius (Javascript, jsFile)
import Util (isDevelopment, widgetFile)
import Yesod
import Yesod.WebSockets (sendTextData, webSockets)

data App = App
  { appPort :: Int,
    appInputDevice :: Device,
    appTVState :: TVar (TVState App)
  }

newtype TVState a = TVState
  { tvPage :: Route a
  }

instance (Eq (Route a)) => Eq (TVState a) where
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
/files/+Texts FilesR GET

-- Routes for the 
/tv TVHomeR GET
/ips AllIPsR GET

-- Other
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
      when isDevelopment $ do
        addScriptRemote "https://pabloproductions.be/LiveJS/live.js"
        addScriptRemote "//cdn.jsdelivr.net/npm/eruda" -- Console for mobile
        toWidgetBody
          [julius|
            window.onload = function() {
              eruda.init();
            };
          |]
      addScript ReconnectingWebSocketJSR
      $(widgetFile "default-layout")
    withUrlRenderer $
      $(hamletFile "templates/default-layout-wrapper.hamlet")

getMobileHomeR :: Handler Html
getMobileHomeR = do
  inputDevice <- getsYesod appInputDevice
  webSockets $ actionsWebSocket inputDevice
  defaultLayout $(widgetFile "home")

getTrackpadR :: Handler Html
getTrackpadR =
  defaultLayout $(widgetFile "trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  defaultLayout $(widgetFile "mouse-pointer")

getKeyboardR :: Handler Html
getKeyboardR =
  defaultLayout $(widgetFile "keyboard")

getFilesR :: [Text] -> Handler Html
getFilesR pieces = do
  home <- liftIO getHomeDirectory
  let currentPath = home </> foldl combine "Videos" (unpack <$> pieces)
  allPaths <- liftIO $ listDirectory currentPath
  (files, directories) <- liftIO $ partitionM (\p -> doesFileExist $ currentPath </> p) allPaths

  let mkPieces :: FilePath -> [Text]
      mkPieces p = pieces ++ [pack p]

  -- Let the tv know what page we're on
  tvStateTVar <- getsYesod appTVState
  liftIO $ atomically $ writeTVar tvStateTVar $ TVState $ FilesR pieces

  defaultLayout $(widgetFile "files")

getInputR :: Handler Html
getInputR =
  defaultLayout $(widgetFile "input")

onChanges :: (Eq a, MonadIO m) => TVar a -> (a -> m ()) -> m b
onChanges tvar action = do
  startValue <- liftIO $ readTVarIO tvar
  action startValue
  loop startValue
  where
    loop currentValue = do
      nextValue <- liftIO $ atomically $ do
        nextValue <- readTVar tvar
        -- If value hasn't changed, retry, which will block until the value changes
        when (currentValue == nextValue) retry
        pure nextValue
      action nextValue
      loop nextValue

getTVHomeR :: Handler Html
getTVHomeR = do
  -- TV has it's own web socket to not interfere with the mobile app
  tvStateTVar <- getsYesod appTVState
  webSockets $ onChanges tvStateTVar $ \tvState -> do
    sendTextData $ pack $ show $ renderRoute $ tvPage tvState

  networkInterfaces <- networkInterfacesShortList <$> liftIO getNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "tv-home")

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces <- liftIO getNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "ips")
  where
    hideZero :: (Show a, Eq a, Bounded a) => a -> String
    hideZero a =
      if a == minBound
        then ""
        else show a

getReconnectingWebSocketJSR :: Handler Javascript
getReconnectingWebSocketJSR =
  withUrlRenderer $(jsFile "templates/reconnecting-websocket.js")

networkInterfacesShortList :: [NetworkInterface] -> [NetworkInterface]
networkInterfacesShortList = filter onShortList
  where
    onShortList :: NetworkInterface -> Bool
    onShortList NetworkInterface {name} = any (`isPrefixOf` name) ["en", "eth", "wl"]

ipV4OrV6WithPort :: Int -> NetworkInterface -> String
ipV4OrV6WithPort port i =
  ( if ipv4 i == IPv4 0
      then show $ ipv6 i
      else show $ ipv4 i
  )
    ++ ":"
    ++ show port

main :: IO ()
main = do
  inputDevice <- mkInputDevice
  tvState <- newTVarIO $ TVState MobileHomeR

  let port = 8080
  let url = "http://localhost:" ++ show port ++ "/"
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    let (path, _params) = renderRoute TVHomeR
    callProcess "xdg-open" [url ++ unpack (intercalate "/" path)]

  warp
    port
    App
      { appPort = port,
        appInputDevice = inputDevice,
        appTVState = tvState
      }
