{-# LANGUAGE DataKinds #-}

module Server where

import Actions (Action, performAction')
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List.Extra (lower)
import Directory (getImagesDir)
import Directory.Directories (RootDirectories)
import Env (ServerEnv (..), ServerM)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived, responseFile)
import Network.Wai.Handler.Warp qualified as Wai
import PVar (readPVar)
import Servant hiding (respond)
import Servant.Server.Generic (AsServerT)
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeExtension, (</>))

type API = NamedRoutes APIRoutes

data APIRoutes mode = APIRoutes
  { apiPostAction :: mode :- "api" :> "action" :> ReqBody '[JSON] Action :> PostNoContent,
    apiGetData :: mode :- "api" :> "data" :> Get '[JSON] RootDirectories,
    apiGetImage :: mode :- "image" :> Capture "imageName" String :> RawM,
    -- -- Must be last, as Servant matches endpoints in order and this captures everything
    apiStatic :: mode :- CaptureAll "pathParts" FilePath :> RawM
  }
  deriving (Generic)

toServantHandler :: ServerEnv -> ServerM a -> Handler a
toServantHandler = flip runReaderT

mkServer :: ServerEnv -> IO (Server API)
mkServer serverEnv = do
  pure $ hoistServer apiProxy (toServantHandler serverEnv) routes

main :: ServerEnv -> IO ()
main serverEnv = do
  server <- mkServer serverEnv
  Wai.run 8080 $ serve apiProxy server

apiProxy :: Proxy API
apiProxy = Proxy

routes :: APIRoutes (AsServerT ServerM)
routes =
  APIRoutes
    { apiPostAction = doAction,
      apiGetData = getData,
      apiGetImage = getImage,
      apiStatic = getStatic
    }
  where
    doAction :: Action -> ServerM NoContent
    doAction action = do
      env <- ask
      performAction' env action
      pure NoContent

    getData :: ServerM RootDirectories
    getData = do
      rootDirs <- asks envRootDirs
      readPVar rootDirs

    getImage :: FilePath -> Request -> (Response -> IO ResponseReceived) -> ServerM ResponseReceived
    getImage imageName _req respond = do
      imagesDir <- getImagesDir
      let imagepath = imagesDir </> imageName
      let extension = case takeExtension imageName of
            '.' : ext -> BS8.pack $ lower ext
            _ -> "jpg"
      liftIO . respond $
        -- `responseFile` automatically adds `Last-Modified` and checks it too
        responseFile
          status200
          [ ("Content-Type", "image/" <> extension),
            ("Cache-Control", "max-age=31536000, public")
          ]
          imagepath
          Nothing

    getStatic :: [FilePath] -> Request -> (Response -> IO ResponseReceived) -> ServerM ResponseReceived
    getStatic pathParts _req respond = do
      let askedPath = "static" </> joinPath pathParts
      exists <- liftIO $ doesFileExist askedPath
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
      liftIO . respond $
        -- `responseFile` automatically adds `Last-Modified` and checks it too
        responseFile
          status200
          ( case contentType' of
              Just ct -> [("Content-Type", ct)]
              Nothing -> []
          )
          finalPath
          Nothing
