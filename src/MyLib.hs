{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MyLib where

import Control.Concurrent.Async (race_)
import Data.List (isPrefixOf)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
import System.Process (callProcess)
import Yesod

data App = App
  { appNetworkInterfaces :: [NetworkInterface],
    appPort :: Int
  }

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/ips AllIPs GET
/mouse/relative MoveMouseR POST
|]

instance Yesod App

addHead :: (MonadWidget m) => Html -> m ()
addHead suffix = do
  setTitle $ "Pablo TV - " <> suffix
  addScriptRemote "https://livejs.com/live.js" -- Maybe some day only include this in dev mode
  toWidgetHead [hamlet|<meta content="text/html; charset=utf-8" http-equiv="Content-Type">|]
  toWidgetHead [hamlet|<meta content="width=device-width, initial-scale=1.0" name="viewport">|]

getHomeR :: Handler Html
getHomeR = do
  networkInterfaces <- networkInterfacesShortList <$> getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $ do
    addHead "Home"
    toWidgetBody
      [julius|
        function throttle(func, timeout = 50){
          let timer;
          let latestFunc;
          return (...args) => {
            latestFunc = func;
            if (!timer) {
              timer = setTimeout(() => { latestFunc.apply(this, args); timer = null; }, timeout);
            }
          };
        }

        function moveMouse(x, y) {
          fetch("@{MoveMouseR}", {
            method: "POST",
            body: JSON.stringify([x, y]),
            headers: {
              "Content-type": "application/json; charset=UTF-8"
            }
          });
        }

        let previousTouch;
        function handleTouchMove(event) {
          const touch = event.touches[0];

          if (previousTouch) {
              const x = touch.pageX - previousTouch.pageX;
              const y = touch.pageY - previousTouch.pageY;
              moveMouse(x, y);
          };
          
          previousTouch = touch;
        }

        document.addEventListener("touchstart", e => previousTouch = e.touches[0]);
        document.addEventListener("touchmove", throttle(handleTouchMove));
      |]
    [whamlet|
      <p>IPs:
      <ul>
          $forall networkInterface <- networkInterfaces
            <li>#{ipV4OrV6WithPort port networkInterface}
          <li>
            <a href=@{AllIPs}>View all...
      
      <button onclick="moveMouse(0, -100);">Move mouse up
      <button onclick="moveMouse(0, 100);">Move mouse down
      <button onclick="moveMouse(-100, 0);">Move mouse left
      <button onclick="moveMouse(100, 0);">Move mouse right
    |]

getAllIPs :: Handler Html
getAllIPs = do
  networkInterfaces <- getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $ do
    addHead "IPs"
    [whamlet|
      <p>Port: #{port}
      <table>
        <thead>
          <tr>
              <th>Name
              <th>IPv4
              <th>IPv6
        <tbody>
          $forall networkInterface <- networkInterfaces
            <tr>
                <td>#{show $ ipv4 networkInterface}
                <td>#{show $ ipv6 networkInterface}
                <td>#{name networkInterface}
    |]

postMoveMouseR :: Handler ()
postMoveMouseR = do
  (x :: Int, y :: Int) <- requireCheckJsonBody
  liftIO $ do
    print $ "Moving: " ++ show x ++ " " ++ show y
    callProcess "ydotool" ["mousemove", "-x", show x, "-y", show y]
  pure ()

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
      warp port $
        App
          { appNetworkInterfaces = ips,
            appPort = port
          }
