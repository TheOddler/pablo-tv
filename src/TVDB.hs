{-# LANGUAGE DerivingVia #-}

module TVDB
  ( TVDBData (..),
    TVDBType (..),
    getInfoFromTVDB,
  )
where

import Autodocodec
import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.Aeson (FromJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (responseHeaders)
import Network.HTTP.Req (GET (GET), HttpConfig, HttpException, HttpResponse (toVanillaResponse), NoReqBody (..), Option, Req, Url, bsResponse, defaultHttpConfig, https, jsonResponse, oAuth2Bearer, req, responseBody, runReq, useURI, (/:), (=:))
import Text.Read (readMaybe)
import Text.URI (mkURI)
import Yesod (ContentType)

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

data RawResponse = RawResponse
  { rawResponseStatus :: Text,
    rawResponseData :: [RawResponseData]
  }
  deriving (Show, Eq)
  deriving (FromJSON) via Autodocodec RawResponse

instance HasCodec RawResponse where
  codec =
    object "RawResponse" $
      RawResponse
        <$> requiredField "status" "Did this succeed?" .= rawResponseStatus
        <*> requiredField "data" "A list of results" .= rawResponseData

data RawResponseData = RawResponseData
  { rawResponseDataId :: Text,
    rawResponseDataOverview :: Maybe Text,
    rawResponseDataOverviews :: KeyMap Text,
    rawResponseDataYear :: String, -- string so we can easily use `readInt` later
    rawResponseDataImageUrl :: Text,
    rawResponseDataRemoteIds :: [RawResponseRemoteId]
  }
  deriving (Show, Eq)

instance HasCodec RawResponseData where
  codec =
    object "RawResponseData" $
      RawResponseData
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

safeReq :: HttpConfig -> Req r -> IO (Either HttpException r)
safeReq c r = try $ runReq c r

getInfoFromTVDB :: BS.ByteString -> Text -> TVDBType -> Maybe Int -> IO (Maybe TVDBData)
getInfoFromTVDB tvdbToken title type' mYear = do
  response <- safeReq defaultHttpConfig $ do
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
            "limit" =: (1 :: Int),
            oAuth2Bearer tvdbToken
          ]
    pure $ listToMaybe $ rawResponseData $ responseBody response

  case response of
    Left err -> do
      putStrLn $ "Failed TVDB request: " <> show err
      pure Nothing
    (Right Nothing) -> pure Nothing
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
        Left err -> putStrLn err
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

downloadImage :: Text -> IO (Either String (ContentType, BS.ByteString))
downloadImage urlT = do
  case useURI =<< mkURI urlT of
    Nothing -> pure $ Left $ "Invalid URL: " <> T.unpack urlT
    Just (Left httpUrl) -> downloadFrom httpUrl
    Just (Right httpsUrl) -> downloadFrom httpsUrl
  where
    downloadFrom :: (Url scheme, Option scheme) -> IO (Either String (ContentType, BS.ByteString))
    downloadFrom (url, options) = do
      imgResponse <-
        safeReq defaultHttpConfig $
          req GET url NoReqBody bsResponse options
      pure $ do
        img <- mapLeft (\err -> "Failed downloading tvdb image: " <> show err) imgResponse
        let headers = responseHeaders $ toVanillaResponse img
        contentType <- lookupHeader "Content-Type" headers
        pure (contentType, responseBody img)

    mapLeft f (Left x) = Left $ f x
    mapLeft _ (Right x) = Right x

    lookupHeader :: (Eq a, Show a, Show b) => a -> [(a, b)] -> Either String b
    lookupHeader x xs = case lookup x xs of
      Just y -> Right y
      Nothing -> Left $ "Failed to find " <> show x <> " in headers: " <> show xs
