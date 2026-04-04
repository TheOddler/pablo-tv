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
import Directory.Directories (RootDirectories)
import Directory.Paths (RawWebPath (..))
import Evdev.Uinput (Device)
import GHC.Conc (TVar)
import IsDevelopment (isDevelopment)
import Logging (LogFunc, LogSlidingWindow, Logger (..))
import Mpris (MediaPlayer)
import PVar (PVar)
import SafeIO (SafeIO (..))
import TVState (TVState)
import Text.Hamlet (hamletFile)
import Transformers (SafeIOT (..))
import Yesod hiding (LogLevel, defaultLayout, replace)
import Yesod qualified
import Yesod.EmbeddedStatic

data App = App
  { appPort :: Int,
    appLogFunc :: LogFunc IO,
    appLogSlidingWindow :: LogSlidingWindow,
    appInputDevice :: Device,
    appGetStatic :: EmbeddedStatic,
    appLastActivePlayer :: TVar (Maybe MediaPlayer),
    appTVState :: TVar TVState,
    appRootDirs :: PVar RootDirectories
  }

mkYesodData
  "App"
  [parseRoutes|
/ HomeR GET POST
/ips AllIPsR GET
/debug DebugR GET
/input InputR GET
/remote RemoteR GET
/dir/+RawWebPath DirectoryR GET

-- Other
/image/+RawWebPath ImageR GET
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
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = pure Nothing
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    isTv <- isTvRequest
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      when isDevelopment $ addScriptRemote "https://pabloproductions.be/LiveJS/live.js"

      when (isDevelopment && not isTv) $ do
        addScriptRemote "//cdn.jsdelivr.net/npm/eruda" -- Console for mobile
        toWidgetBody
          [julius|
            window.onload = function() {
              eruda.init();
            };
          |]

      addScript $ StaticR static_reconnecting_websocket_js
      addStylesheet $ StaticR static_fontawesome_css_all_min_css

      widget
    withUrlRenderer $
      $(hamletFile "templates/page-wrapper.hamlet")

isTvRequest :: (MonadHandler m) => m Bool
isTvRequest = do
  mHostHeader <- lookupHeader "Host"
  let isHost h = maybe False (h `BS.isInfixOf`) mHostHeader
  pure $ any isHost ["localhost", "127.0.0.1", "0:0:0:0:0:0:0:1"]

instance Logger Handler where
  putLogMsg msg = do
    logFunc <- getsYesod appLogFunc
    liftIO $ logFunc msg

instance SafeIO Handler where
  runIOSafely = runSafeIOT . runIOSafely
  unsafePinkyPromiseThisIsSafe = runSafeIOT . unsafePinkyPromiseThisIsSafe
  getCurrentTime = runSafeIOT getCurrentTime
  getModificationTime = runSafeIOT . getModificationTime
  getHomeDirectory = runSafeIOT getHomeDirectory
