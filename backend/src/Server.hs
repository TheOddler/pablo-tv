{-# LANGUAGE DataKinds #-}

module Server where

import Actions (Action, performAction')
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (stripPrefix)
import Data.List.Extra (lower)
import Directory (getImagesDir)
import Directory.Directories (RootDirectories)
import Env (ServerEnv (..), ServerM)
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType, hLastModified, status200, status304, status412)
import Network.HTTP.Types.Header (hETag)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import NetworkInfo (NetworkInfo, getNetworkInfo)
import PVar (readPVar)
import Servant hiding (respond)
import Servant.Server.Generic (AsServerT)
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeExtension, (</>))

type API = NamedRoutes APIRoutes

data APIRoutes mode = APIRoutes
  { apiPostAction :: mode :- "api" :> "action" :> ReqBody '[JSON] Action :> PostNoContent,
    apiGetData :: mode :- "api" :> "data" :> Get '[JSON] RootDirectories,
    apiGetNetworkInfo :: mode :- "api" :> "network" :> Get '[JSON] NetworkInfo,
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
  Wai.run 8080 $ etagMiddleware $ serve apiProxy server

apiProxy :: Proxy API
apiProxy = Proxy

routes :: APIRoutes (AsServerT ServerM)
routes =
  APIRoutes
    { apiPostAction = doAction,
      apiGetData = getData,
      apiGetNetworkInfo = getNetworkInfo,
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

    getImage :: FilePath -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ServerM Wai.ResponseReceived
    getImage imageName _req respond = do
      imagesDir <- getImagesDir
      let imagepath = imagesDir </> imageName
      let extension = case takeExtension imageName of
            '.' : ext -> BS8.pack $ lower ext
            _ -> "jpg"
      liftIO . respond $
        -- `responseFile` automatically adds `Last-Modified` and checks it too
        Wai.responseFile
          status200
          [ ("Content-Type", "image/" <> extension),
            ("Cache-Control", "max-age=31536000, public")
          ]
          imagepath
          Nothing

    getStatic :: [FilePath] -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ServerM Wai.ResponseReceived
    getStatic pathParts _req respond = do
      frontendDir <- asks envFrontend
      let askedPath = frontendDir </> joinPath pathParts
      exists <- liftIO $ doesFileExist askedPath
      let finalPath =
            if exists
              then askedPath
              else frontendDir </> "index.html"

      let contentType' :: Maybe BS.ByteString
          contentType' = case takeExtension finalPath of
            ".html" -> Just "text/html; charset=utf-8"
            ".css" -> Just "text/css"
            ".js" -> Just "application/javascript"
            '.' : ext -> Just $ "application/" <> BS8.pack (lower ext)
            _ -> Nothing
      let contentTypeHeader = case contentType' of
            Nothing -> []
            Just ct -> [(hContentType, ct)]

      -- When we're in the Nix store the mod time is set to posix+1s, so if that's the case we should set the ETag as the Last-Modified header is useless.
      let nixStorePath = stripPrefix "/nix/store/" finalPath
      let nixStoreEtag = case nixStorePath of
            Nothing -> Nothing
            Just nsp -> Just ("\"" <> BS8.pack nsp <> "\"")
      let eTagHeader = case nixStoreEtag of
            Nothing -> []
            -- `responseFile` automatically adds `Last-Modified` header, so remove it if we add an etag
            Just etag -> [(hETag, etag), (hLastModified, "")]
      liftIO . respond $
        Wai.responseFile
          status200
          (contentTypeHeader ++ eTagHeader)
          finalPath
          Nothing

-- | If the response has an `ETag` header, and the request has `If-Match` or `If-None-Match` header, this middleware potentially sends a 304 or 412
etagMiddleware :: Application -> Application
etagMiddleware appl req respond = appl req $ \response -> do
  let responseHeaders = Wai.responseHeaders response
  let mEtag = findEtag responseHeaders
  let respondNormal = respond response
  let respondWithStatus st = respond $ Wai.responseLBS st responseHeaders ""
  case mEtag of
    -- No etag, so respond normal
    Nothing -> respondNormal
    Just etag -> do
      -- There's an etag in our response, so check if the request
      let go requestHeaders =
            case requestHeaders of
              [] -> respondNormal
              ("If-Match", expectedEtag) : _ | expectedEtag /= etag -> respondWithStatus status412
              ("If-Match", _) : _ -> respondNormal
              ("If-None-Match", expectedEtag) : _ | expectedEtag == etag -> respondWithStatus status304
              ("If-None-Match", _) : _ -> respondNormal
              _ : rest -> go rest
      go req.requestHeaders
  where
    findEtag = \case
      [] -> Nothing
      (h, v) : _ | h == hETag -> Just v
      _ : rest -> findEtag rest
