{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (isNothing, listToMaybe)
import Directory (RootDirectories)
import Directory.Directories (DirectoryName, RootDirectoryLocation)
import Evdev.Uinput (Device)
import GHC.Conc (TVar)
import GHC.Utils.Misc (sortWith)
import IsDevelopment (isDevelopment)
import Logging (LogFunc, Logger (..))
import Network.Info (getNetworkInterfaces)
import PVar (PVar)
import TVDB (TVDBToken)
import TVState (TVState)
import Text.Hamlet (hamletFile)
import Util (networkInterfaceWorthiness, showIpV4OrV6WithPort, widgetFile)
import Yesod hiding (LogLevel, defaultLayout, replace)
import Yesod qualified
import Yesod.EmbeddedStatic

data App = App
  { appPort :: Int,
    appLogFunc :: LogFunc IO,
    appTVDBToken :: Maybe TVDBToken,
    appInputDevice :: Device,
    appGetStatic :: EmbeddedStatic,
    appTVState :: TVar TVState,
    appRootDirs :: PVar RootDirectories
  }

type DirectoryNames = [DirectoryName]

mkYesodData
  "App"
  [parseRoutes|
/ HomeR GET POST
/ips AllIPsR GET
/input InputR GET
/remote RemoteR GET
/dir DirectoryHomeR GET
/dir/#RootDirectoryLocation/+DirectoryNames DirectoryR GET

-- Other
/image/#RootDirectoryLocation/+DirectoryNames ImageR GET
/static StaticR EmbeddedStatic appGetStatic
|]

mkEmbeddedStatic
  False
  "embeddedStatic"
  [ embedFile "static/reconnecting-websocket.js",
    embedFile "static/fontawesome/css/all.min.css",
    embedFile "static/fontawesome/webfonts/fa-brands-400.ttf",
    embedFile "static/fontawesome/webfonts/fa-brands-400.woff2",
    embedFile "static/fontawesome/webfonts/fa-regular-400.ttf",
    embedFile "static/fontawesome/webfonts/fa-regular-400.woff2",
    embedFile "static/fontawesome/webfonts/fa-solid-900.ttf",
    embedFile "static/fontawesome/webfonts/fa-solid-900.woff2",
    embedFile "static/images/apple-tv-plus.png",
    embedFile "static/images/netflix.png",
    embedFile "static/images/youtube.png"
  ]

instance Yesod App where
  addStaticContent = embedStaticContent appGetStatic StaticR Right
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    isTv <- isTvRequest
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      -- when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"

      when (isDevelopment && not isTv) $ do
        addScriptRemote "//cdn.jsdelivr.net/npm/eruda" -- Console for mobile
        toWidgetBody
          [julius|
            window.onload = function() {
              eruda.init();
            };
          |]

      networkInterfaces <- liftIO getNetworkInterfaces
      let mNetworkInterface =
            listToMaybe $
              sortWith networkInterfaceWorthiness networkInterfaces
      port <- getsYesod appPort
      inReadOnlyMode <- isNothing <$> getsYesod appTVDBToken

      addScript $ StaticR static_reconnecting_websocket_js
      addStylesheet $ StaticR static_fontawesome_css_all_min_css
      -- currentRoute <- fromMaybe HomeR <$> getCurrentRoute
      -- currentUrlBS <- toUrlRel currentRoute
      -- let currentUrl :: Text
      --     currentUrl = decodeUtf8Lenient $ BS.toStrict currentUrlBS
      $(widgetFile "default")
    withUrlRenderer $
      $(hamletFile "templates/page-wrapper.hamlet")

isTvRequest :: Handler Bool
isTvRequest = do
  mHostHeader <- lookupHeader "Host"
  let isHost h = maybe False (h `BS.isInfixOf`) mHostHeader
  pure $ any isHost ["localhost", "127.0.0.1", "0:0:0:0:0:0:0:1"]

defaultLayout :: Html -> Widget -> Handler Html
defaultLayout title widget = Yesod.defaultLayout $ do
  setTitle $ title <> " - Pablo TV"
  widget

instance Logger Handler where
  putLogBS lvl msg = do
    logFunc <- getsYesod appLogFunc
    liftIO $ logFunc lvl msg
