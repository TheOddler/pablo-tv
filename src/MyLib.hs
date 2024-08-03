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
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
  networkInterfaces <- networkInterfacesShortList <$> getsYesod appNetworkInterfaces
  defaultLayout $ do
    setTitle "Haskell TV"
    addScriptRemote "https://livejs.com/live.js" -- Maybe some day only include this in dev mode
    [whamlet|
      <p>IPs:
      <ul>
          $forall networkInterface <- networkInterfaces
            <li>#{prettyShowNetworkInterface networkInterface}
    |]

networkInterfacesShortList :: [NetworkInterface] -> [NetworkInterface]
networkInterfacesShortList = filter onShortList
  where
    onShortList :: NetworkInterface -> Bool
    onShortList i = i `startsWith` "en" || i `startsWith` "eth" || i `startsWith` "wl"
    startsWith = flip isPrefixOf . name

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
