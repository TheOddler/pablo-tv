{-# LANGUAGE DataKinds #-}

module Server where

import Actions (Action, performAction')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks)
import Directory (getImagesDir)
import Directory.Directories (RootDirectories)
import Foundation (App (..))
import GHC.Generics (Generic)
import ImageScraper (ImageScraper (..), tryFindImageIO)
import Logging (Logger (..))
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp qualified as Wai
import PVar (readPVar)
import SafeIO (SafeIO (..))
import Servant
import Servant.Server.Generic (AsServerT)
import Transformers (SafeIOT (..))

type API = NamedRoutes APIRoutes

data APIRoutes mode = APIRoutes
  { apiActions :: mode :- "api" :> "action" :> ReqBody '[JSON] Action :> PostNoContent,
    apiData :: mode :- "api" :> "data" :> Get '[JSON] RootDirectories,
    apiImages :: mode :- "api" :> "image" :> Raw,
    -- Must be last, as Servant matches endpoints in order and this captures everything
    apiStatic :: mode :- Raw
  }
  deriving (Generic)

-- | Currently using the App from Yesod
type ServerEnv = App

type ServerM = ReaderT ServerEnv Servant.Handler

instance Logger ServerM where
  putLogMsg msg = do
    logFunc <- asks appLogFunc
    liftIO $ logFunc msg

instance {-# OVERLAPPING #-} SafeIO ServerM where
  runIOSafely = runSafeIOT . runIOSafely
  unsafePinkyPromiseThisIsSafe = runSafeIOT . unsafePinkyPromiseThisIsSafe
  getCurrentTime = runSafeIOT getCurrentTime
  getModificationTime = runSafeIOT . getModificationTime
  getHomeDirectory = runSafeIOT getHomeDirectory
  randomFileNameSuffix = runSafeIOT randomFileNameSuffix

instance ImageScraper ServerM where
  tryFindImage = tryFindImageIO

toServantHandler :: ServerEnv -> ServerM a -> Servant.Handler a
toServantHandler = flip runReaderT

mkServer :: ServerEnv -> IO (Server API)
mkServer serverEnv = do
  pure $ hoistServer apiProxy (toServantHandler serverEnv) routes

main :: ServerEnv -> IO ()
main serverEnv = do
  server <- mkServer serverEnv
  -- run 8081 $ simpleCors $ serve apiProxy server
  Wai.run 8081 $ serve apiProxy server

apiProxy :: Proxy API
apiProxy = Proxy

routes :: APIRoutes (AsServerT ServerM)
routes =
  APIRoutes
    { apiActions = \action -> do
        env <- ask
        performAction' env action
        pure NoContent,
      apiData = do
        rootDirs <- asks appRootDirs
        readPVar rootDirs,
      apiImages = Tagged $ \req resp -> do
        imagesDir <- runSafeIOT getImagesDir
        let stSet = defaultWebAppSettings imagesDir
        staticApp stSet req resp,
      apiStatic = serveDirectoryWebApp "static"
    }
