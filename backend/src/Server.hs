{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import Actions (Action, performAction')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List.Extra (lower)
import Data.Maybe (catMaybes)
import Directory (getImagesDir)
import Directory.Directories (RootDirectories)
import Foundation (App (..))
import GHC.Generics (Generic)
import ImageScraper (ImageScraper (..), tryFindImageIO)
import Logging (Logger (..))
import Network.HTTP.Types (status200)
import Network.Wai (responseFile)
import Network.Wai.Handler.Warp qualified as Wai
import PVar (readPVar)
import SafeIO (SafeIO (..))
import Servant hiding (respond)
import Servant.Server.Generic (AsServerT)
import Servant.Types.SourceT qualified as S
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeExtension, (</>))
import Transformers (SafeIOT (..))

type API = NamedRoutes APIRoutes

data APIRoutes mode = APIRoutes
  { apiActions :: mode :- "api" :> "action" :> ReqBody '[JSON] Action :> PostNoContent,
    apiData :: mode :- "api" :> "data" :> Get '[JSON] RootDirectories,
    apiImages ::
      mode
        :- "api"
          :> "image"
          :> Capture "imageName" String
          :> StreamGet
               NoFraming
               OctetStream
               ( Headers
                   '[ Header "Content-Type" String,
                      Header "Cache-Control" String
                    ]
                   (SourceIO BS.ByteString)
               ),
    -- Must be last, as Servant matches endpoints in order and this captures everything
    apiStatic :: mode :- CaptureAll "pathParts" FilePath :> Raw
  }
  deriving (Generic)

-- | Currently using the App from Yesod
type ServerEnv = App

type ServerM = ReaderT ServerEnv Handler

instance Logger ServerM where
  putLogMsg msg = do
    logFunc <- asks appLogFunc
    liftIO $ logFunc msg

instance SafeIO Handler where
  runIOSafely = runSafeIOT . runIOSafely
  unsafePinkyPromiseThisIsSafe = runSafeIOT . unsafePinkyPromiseThisIsSafe
  getCurrentTime = runSafeIOT getCurrentTime
  getModificationTime = runSafeIOT . getModificationTime
  getHomeDirectory = runSafeIOT getHomeDirectory
  randomFileNameSuffix = runSafeIOT randomFileNameSuffix

instance ImageScraper ServerM where
  tryFindImage = tryFindImageIO

toServantHandler :: ServerEnv -> ServerM a -> Handler a
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
      apiImages = \imageName -> do
        imagesDir <- runSafeIOT getImagesDir
        let filepath = imagesDir </> imageName
        let extension = case takeExtension imageName of
              '.' : ext -> lower ext
              _ -> "jpg"
        let contentTypeHeader = "image/" ++ extension
        let cacheControl = "max-age=31536000, public"
        pure $
          addHeader contentTypeHeader $
            addHeader cacheControl $
              S.readFile filepath,
      apiStatic = \pathParts -> Tagged $ \_req respond -> do
        let askedPath = "static" </> joinPath pathParts
        exists <- doesFileExist askedPath
        let finalPath =
              if exists
                then askedPath
                else "static" </> "index.html"
        let contentType' :: Maybe BS.ByteString
            contentType' = case takeExtension finalPath of
              ".html" -> Just "text/html; charset=utf-8"
              ".css" -> Just "text/css"
              ".js" -> Just "application/javascript"
              '.' : ext -> Just $ "application/" <> BS8.pack (lower ext)
              _ -> Nothing
        respond $
          responseFile
            status200
            ( catMaybes
                [ case contentType' of
                    Just ct -> Just ("Content-Type", ct)
                    Nothing -> Nothing
                ]
            )
            finalPath
            Nothing
    }
