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
  defaultLayout $ do
    addHead "Home"
    [whamlet|
      <p>IPs:
      <ul>
          $forall networkInterface <- networkInterfaces
            <li>#{prettyShowNetworkInterface networkInterface}
    |]

getAllIPs :: Handler Html
getAllIPs = do
  networkInterfaces <- getsYesod appNetworkInterfaces
  defaultLayout $ do
    addHead "IPs"
    [whamlet|
      <p>IPs:
      <table>
        <thead>
          <tr>
              <th>Name
              <th>IPv4
              <th>IPv6
        <tbody>
          $forall networkInterface <- networkInterfaces
            <tr>
                <td>#{name networkInterface}
                <td>#{show $ ipv4 networkInterface}
                <td>#{show $ ipv6 networkInterface}
    |]

networkInterfacesShortList :: [NetworkInterface] -> [NetworkInterface]
networkInterfacesShortList = filter onShortList
  where
    onShortList :: NetworkInterface -> Bool
    onShortList NetworkInterface {name} = any (`isPrefixOf` name) ["en", "eth", "wl"]

prettyShowNetworkInterface :: NetworkInterface -> String
prettyShowNetworkInterface i =
  name i
    ++ ": "
    ++ if ipv4 i == IPv4 0
      then show $ ipv6 i
      else show $ ipv4 i

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
