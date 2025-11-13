{-# LANGUAGE DerivingVia #-}

module TVDB
  ( TVDBData (..),
    TVDBType (..),
    TVDBApiKey (..),
    TVDBToken,
    getToken,
    getInfoFromTVDB,
  )
where

import Autodocodec
import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Logging (LogLevel (..), Logger, putLog)
import Network.HTTP.Client (responseHeaders)
import Network.HTTP.Req
  ( GET (GET),
    HttpResponse (toVanillaResponse),
    NoReqBody (..),
    Option,
    POST (..),
    ReqBodyJson (..),
    Url,
    bsResponse,
    defaultHttpConfig,
    https,
    jsonResponse,
    oAuth2Bearer,
    req,
    responseBody,
    useURI,
    (/:),
    (=:),
  )
import Network.HTTP.Req qualified as HTTP
import Text.Read (readMaybe)
import Text.URI (mkURI)
import Util (mapLeft)
import Yesod (ContentType, MonadIO (..))

newtype TVDBApiKey = TVDBApiKey {unTVDBApiKey :: String}
  deriving (ToJSON) via Autodocodec TVDBApiKey

instance HasCodec TVDBApiKey where
  codec =
    object "LoginData" $
      TVDBApiKey
        <$> requiredField "apikey" "The API Key" .= unTVDBApiKey

newtype TVDBToken = TVDBToken {unTVDBToken :: BS.ByteString}

instance HasCodec TVDBToken where
  codec = object "LoginData" $ TVDBToken <$> tokenCodec
    where
      tokenCodec :: ObjectCodec TVDBToken BS.ByteString
      tokenCodec = dimapCodec encodeUtf8 (decodeUtf8Lenient . unTVDBToken) $ requiredField "token" "The API Key"

data RawResponse a = RawResponse
  { rawResponseStatus :: String,
    rawResponseData :: a
  }
  deriving (Show, Eq)
  deriving (FromJSON) via Autodocodec (RawResponse a)

instance (HasCodec a) => HasCodec (RawResponse a) where
  codec =
    object "RawResponse" $
      RawResponse
        <$> requiredField "status" "Did this succeed?" .= rawResponseStatus
        <*> requiredField "data" "A list of results" .= rawResponseData

getToken :: (MonadIO m, Logger m) => TVDBApiKey -> m (Maybe TVDBToken)
getToken apiKey = do
  responseOrErr <-
    runReqSafe defaultHttpConfig $
      responseBody
        <$> req POST (https "api4.thetvdb.com" /: "v4" /: "login") (ReqBodyJson apiKey) jsonResponse mempty
  case responseOrErr of
    Left err -> do
      putLog Error $ "Error while getting TVDB Token: " ++ show err
      pure Nothing
    Right response ->
      case rawResponseStatus response of
        "success" -> pure $ Just $ rawResponseData response
        status -> do
          putLog Error $ "Failed getting TVDB Token: " ++ show status
          pure Nothing

data TVDBType = TVDBTypeSeries | TVDBTypeMovie
  deriving (Show, Eq)

data TVDBData = TVDBData
  { tvdbDataDescription :: Maybe Text,
    tvdbDataYear :: Maybe Int,
    tvdbDataImage :: Maybe (ContentType, BS.ByteString),
    tvdbDataId :: Text,
    tvdbDataImdb :: Maybe Text,
    tvdbDataTmdb :: Maybe Text
  }
  deriving (Show, Eq)

data SearchResponseData = SearchResponseData
  { rawResponseDataId :: Text,
    rawResponseDataOverview :: Maybe Text,
    rawResponseDataOverviews :: KeyMap Text,
    rawResponseDataYear :: String, -- string so we can easily use `readInt` later
    rawResponseDataImageUrl :: Text,
    rawResponseDataRemoteIds :: [RawResponseRemoteId]
  }
  deriving (Show, Eq)

instance HasCodec SearchResponseData where
  codec =
    object "SearchResponseData" $
      SearchResponseData
        <$> requiredField "id" "Unique identifier" .= rawResponseDataId
        <*> optionalField "overview" "The description" .= rawResponseDataOverview
        <*> optionalFieldWithDefault "overviews" mempty "A mapping from language to descriptions" .= rawResponseDataOverviews
        <*> requiredField "year" "The year when the series or movie was released" .= rawResponseDataYear
        <*> requiredField "image_url" "A link to a poster image" .= rawResponseDataImageUrl
        <*> optionalFieldWithDefault "remote_ids" [] "A link to a poster image" .= rawResponseDataRemoteIds

data RawResponseRemoteId = RawResponseRemoteId
  { rawResponseRemoteId :: Text,
    rawResponseRemoteSourceName :: Text
  }
  deriving (Show, Eq)

instance HasCodec RawResponseRemoteId where
  codec =
    object "RawResponseRemoteId" $
      RawResponseRemoteId
        <$> requiredField "id" "The remote id" .= rawResponseRemoteId
        <*> requiredField "sourceName" "Either `IMDB` or `TheMovieDB.com`, among other irrelevant ones for us" .= rawResponseRemoteSourceName

getInfoFromTVDB :: (MonadIO m, Logger m) => TVDBToken -> Text -> TVDBType -> Maybe Text -> Maybe Int -> m (Maybe TVDBData)
getInfoFromTVDB (TVDBToken tvdbToken) title type' mRemoteId mYear = do
  response <- runReqSafe defaultHttpConfig $ do
    response <-
      req GET (https "api4.thetvdb.com" /: "v4" /: "search") NoReqBody jsonResponse $
        mconcat
          [ "query" =: title,
            "type" =: case type' of
              TVDBTypeSeries -> "series" :: Text
              TVDBTypeMovie -> "movie",
            case mYear of
              Just year -> "year" =: year
              Nothing -> mempty,
            case mRemoteId of
              Just remoteId -> "remote_id" =: remoteId
              Nothing -> mempty,
            "limit" =: (1 :: Int),
            oAuth2Bearer tvdbToken
          ]
    pure $ listToMaybe $ rawResponseData $ responseBody response

  let debugInfo = (title, type', mRemoteId, mYear)

  case response of
    Left err -> do
      putLog Error $ "Failed TVDB request: " <> show err <> "; " <> show debugInfo
      pure Nothing
    (Right Nothing) -> do
      putLog Warning $ "TVDB request return nothing. " <> show debugInfo
      pure Nothing
    (Right (Just firstData)) -> do
      let imgUrl = rawResponseDataImageUrl firstData
      let lookupRemoteId name =
            lookup
              name
              [ (rawId.rawResponseRemoteSourceName, rawId.rawResponseRemoteId)
                | rawId <- rawResponseDataRemoteIds firstData
              ]
      imgOrError <- downloadImage imgUrl
      case imgOrError of
        Right _ -> pure ()
        Left err -> putLog Error err
      pure $
        Just $
          TVDBData
            { tvdbDataDescription = KeyMap.lookup "eng" firstData.rawResponseDataOverviews <|> rawResponseDataOverview firstData,
              tvdbDataYear = readMaybe $ rawResponseDataYear firstData,
              tvdbDataImage = case imgOrError of
                Left _ -> Nothing
                Right img -> Just img,
              tvdbDataId = rawResponseDataId firstData,
              tvdbDataImdb = lookupRemoteId "IMDB",
              tvdbDataTmdb = lookupRemoteId "TheMovieDB.com"
            }

downloadImage :: (MonadIO m) => Text -> m (Either String (ContentType, BS.ByteString))
downloadImage urlT = do
  case useURI =<< mkURI urlT of
    Nothing -> pure $ Left $ "Invalid URL: " <> T.unpack urlT
    Just (Left httpUrl) -> downloadFrom httpUrl
    Just (Right httpsUrl) -> downloadFrom httpsUrl
  where
    downloadFrom :: (MonadIO m) => (Url scheme, Option scheme) -> m (Either String (ContentType, BS.ByteString))
    downloadFrom (url, options) = do
      imgResponse <-
        runReqSafe defaultHttpConfig $
          req GET url NoReqBody bsResponse options
      pure $ do
        img <- mapLeft (\err -> "Failed downloading tvdb image: " <> show err) imgResponse
        let headers = responseHeaders $ toVanillaResponse img
        contentType <- lookupHeader "Content-Type" headers
        pure (contentType, responseBody img)

    lookupHeader :: (Eq a, Show a, Show b) => a -> [(a, b)] -> Either String b
    lookupHeader x xs = case lookup x xs of
      Just y -> Right y
      Nothing -> Left $ "Failed to find " <> show x <> " in headers: " <> show xs

runReqSafe :: (MonadIO m) => HTTP.HttpConfig -> HTTP.Req a -> m (Either HTTP.HttpException a)
runReqSafe c = liftIO . try . HTTP.runReq c
