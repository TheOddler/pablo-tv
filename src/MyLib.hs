{-# LANGUAGE CPP #-}
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
import Text.Hamlet (hamletFile)
import Util (isDevelopment, widgetFile)
import Yesod

data App = App
  { appNetworkInterfaces :: [NetworkInterface],
    appPort :: Int
  }

mkYesod
  "App"
  [parseRoutes|
-- Pages:
/ HomeR GET
/ips AllIPsR GET
/trackpad TrackpadR GET
/pointer MousePointerR GET

-- Functions:
/mouse/relative MoveMouseR POST
/mouse/point MousePointR POST
/mouse/click ClickMouseR POST
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
      -- when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"
      $(widgetFile "default-layout")
    withUrlRenderer $
      $(hamletFile "templates/default-layout-wrapper.hamlet")

getHomeR :: Handler Html
getHomeR = do
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

postMoveMouseR :: Handler ()
postMoveMouseR = do
  (x :: Int, y :: Int) <- requireCheckJsonBody
  liftIO $ do
    putStrLn $ "Moving: " ++ show x ++ " " ++ show y
    callProcess "ydotool" ["mousemove", "-x", show x, "-y", show y]
  pure ()

-- | Point the mouse relative to the center of the screen
-- So (0,0) is the center of the screen
-- Then, assuming the screen is wider then high, (0, 1) means middle top
-- and (1, 0) means equally far to right right as the top is from the center,
-- so something like (1.6, 0) would be center outer right
-- Does that make sense?
-- It's meant to be used with the mouse pointer app.
postMousePointR :: Handler ()
postMousePointR = do
  (x :: Float, y :: Float) <- requireCheckJsonBody

  let screenHalfSize = 690 / 2 -- For some reason ydotool thinks the screen is 690 high...
      centerX = 1100 / 2 -- For some reason ydotool thinks the screen is 1100 wide?
      centerY = 690 / 2 -- For some reason ydotool thinks the screen is 690 high...
      pointerX :: Int
      pointerX = round $ centerX + x * screenHalfSize
      pointerY :: Int
      pointerY = round $ centerY + y * screenHalfSize

  liftIO $ do
    putStrLn $ "Pointing: " ++ show pointerX ++ " " ++ show pointerY
    callProcess "ydotool" ["mousemove", "--absolute", "-x", show pointerX, "-y", show pointerY]
  pure ()

postClickMouseR :: Handler ()
postClickMouseR = liftIO $ do
  putStrLn "Clicking"
  callProcess "ydotool" ["click", "0xC0"]

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
