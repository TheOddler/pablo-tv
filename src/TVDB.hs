{-# LANGUAGE DerivingVia #-}

module TVDB
  ( TVDBData (..),
    TVDBType (..),
    getInfoFromTVDB,
  )
where

import Autodocodec
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (responseHeaders)
import Network.HTTP.Req (GET (GET), HttpResponse (toVanillaResponse), NoReqBody (..), bsResponse, defaultHttpConfig, https, jsonResponse, oAuth2Bearer, req, responseBody, runReq, useURI, (/:), (=:))
import Text.Read (readMaybe)
import Text.URI (mkURI)
import Yesod (ContentType)

data TVDBType = TVDBTypeSeries | TVDBTypeMovie
  deriving (Show, Eq)

data TVDBData = TVDBData
  { tvdbDataName :: Text,
    tvdbDataDescription :: Text,
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
    rawResponseDataName :: Text,
    rawResponseDataOverview :: Text,
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
        <*> requiredField "name" "The name of this series or movie" .= rawResponseDataName
        <*> requiredField "overview" "The description" .= rawResponseDataOverview
        <*> requiredField "year" "The year when the series or movie was released" .= rawResponseDataYear
        <*> requiredField "image_url" "A link to a poster image" .= rawResponseDataImageUrl
        <*> requiredField "remote_ids" "A link to a poster image" .= rawResponseDataRemoteIds

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

getInfoFromTVDB :: BS.ByteString -> Text -> TVDBType -> Maybe Int -> IO (Maybe TVDBData)
getInfoFromTVDB tvdbToken title type' year = do
  mFirstData <- runReq defaultHttpConfig $ do
    response <-
      req GET (https "api4.thetvdb.com" /: "v4" /: "search") NoReqBody jsonResponse $
        mconcat
          [ "query" =: title,
            "type" =: case type' of
              TVDBTypeSeries -> "series" :: Text
              TVDBTypeMovie -> "movie",
            "year" =: year,
            "limit" =: (1 :: Int),
            oAuth2Bearer tvdbToken
          ]
    pure $ listToMaybe $ rawResponseData $ responseBody response

  case mFirstData of
    Nothing -> pure Nothing
    Just firstData -> do
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
            { tvdbDataName = rawResponseDataName firstData,
              tvdbDataDescription = rawResponseDataOverview firstData,
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
    Just (Left httpUrl) -> Right <$> downloadFrom httpUrl
    Just (Right httpsUrl) -> Right <$> downloadFrom httpsUrl
  where
    downloadFrom (url, options) = do
      img <-
        runReq defaultHttpConfig $
          req GET url NoReqBody bsResponse options
      let headers = responseHeaders $ toVanillaResponse img
          contentType = fromMaybe "image/jpeg" $ lookup "Content-Type" headers
      pure (contentType, responseBody img)
