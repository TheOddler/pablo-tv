{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LibMain where

import Actions (actionsWebSocket, mkInputDevice)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.List (isPrefixOf)
import Data.Text (intercalate, pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Evdev.Uinput (Device)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Text.Julius (Javascript, jsFile)
import Util (isDevelopment, widgetFile)
import Yesod
import Yesod.WebSockets (sendTextData, webSockets)

data App = App
  { appNetworkInterfaces :: [NetworkInterface],
    appPort :: Int,
    appInputDevice :: Device
  }

mkYesod
  "App"
  [parseRoutes|
-- Routes for the mobile app
/ MobileHomeR GET
/trackpad TrackpadR GET
/pointer MousePointerR GET
/keyboard KeyboardR GET

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

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces <- getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "ips")
  where
    hideZero :: (Show a, Eq a, Bounded a) => a -> String
    hideZero a =
      if a == minBound
        then ""
        else show a

getTrackpadR :: Handler Html
getTrackpadR =
  defaultLayout $(widgetFile "trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  defaultLayout $(widgetFile "mouse-pointer")

getKeyboardR :: Handler Html
getKeyboardR =
  defaultLayout $(widgetFile "keyboard")

getTVHomeR :: Handler Html
getTVHomeR = do
  -- TV has it's own web socket to not interfere with the mobile app
  webSockets $ forever $ do
    time <- liftIO getCurrentTime
    sendTextData $ pack $ show time
    liftIO $ threadDelay 1_000_000

  networkInterfaces <- networkInterfacesShortList <$> getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "tv-home")

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
  ips <- getNetworkInterfaces
  let port = 8080
  let url = "http://localhost:" ++ show port ++ "/"
  putStrLn $ "Running on port " ++ show port ++ " - " ++ url
  putStrLn $ "Development mode: " ++ show isDevelopment

  -- Only open the browser automatically in production because it;s annoying in
  -- development as it opens a new tab every time the server restarts.
  when (not isDevelopment) $ do
    let (path, _params) = renderRoute TVHomeR
    callProcess "xdg-open" [url ++ unpack (intercalate "/" path)]

  warp port $
    App
      { appNetworkInterfaces = ips,
        appPort = port,
        appInputDevice = inputDevice
      }
