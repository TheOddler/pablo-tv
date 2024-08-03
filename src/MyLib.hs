{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MyLib where

import Data.List (isPrefixOf)
import Network.Info (IPv4 (..), NetworkInterface (..), getNetworkInterfaces)
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
|]

instance Yesod App

addHead :: (MonadWidget m) => Html -> m ()
addHead suffix = do
  setTitle $ "Pablo TV - " <> suffix
  addScriptRemote "https://livejs.com/live.js" -- Maybe some day only include this in dev mode

getHomeR :: Handler Html
getHomeR = do
  networkInterfaces <- networkInterfacesShortList <$> getsYesod appNetworkInterfaces
  port <- getsYesod appPort
  defaultLayout $ do
    addHead "Home"
    [whamlet|
      <p>IPs:
      <ul>
          $forall networkInterface <- networkInterfaces
            <li>#{ipV4OrV6WithPort port networkInterface}
          <li><a href=@{AllIPs}>View all...
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
  ips <- getNetworkInterfaces
  let port = 8080
  putStrLn "Running on port 8080 - http://localhost:8080/"
  warp port $
    App
      { appNetworkInterfaces = ips,
        appPort = port
      }
