{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MyLib where

import Actions (actionsWebSocket)
import Control.Concurrent.Async (race_)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import System.Process (callProcess)
import Text.Hamlet (hamletFile)
import Util (isDevelopment, widgetFile)
import Yesod
import Yesod.WebSockets (webSockets)

data App = App
  { appNetworkInterfaces :: [NetworkInterface],
    appPort :: Int
  }

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/ips AllIPsR GET
/trackpad TrackpadR GET
/pointer MousePointerR GET
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
      $(widgetFile "default-layout")
    withUrlRenderer $
      $(hamletFile "templates/default-layout-wrapper.hamlet")

getHomeR :: Handler Html
getHomeR = do
  webSockets actionsWebSocket
  networkInterfaces <- networkInterfacesShortList <$> getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "home")

getAllIPsR :: Handler Html
getAllIPsR = do
  networkInterfaces <- getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $(widgetFile "ips")

getTrackpadR :: Handler Html
getTrackpadR =
  defaultLayout $(widgetFile "trackpad")

getMousePointerR :: Handler Html
getMousePointerR =
  defaultLayout $(widgetFile "mouse-pointer")

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
  race_ yDoToolDaemon server
  where
    yDoToolDaemon = callProcess "ydotoold" []
    server = do
      ips <- getNetworkInterfaces
      let port = 8080
      putStrLn "Running on port 8080 - http://localhost:8080/"
      putStrLn $ "Development mode: " ++ show isDevelopment
      warp port $
        App
          { appNetworkInterfaces = ips,
            appPort = port
          }
