{-# LANGUAGE DerivingVia #-}

module TVDB where

import Autodocodec
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req (GET (GET), NoReqBody (..), defaultHttpConfig, https, jsonResponse, oAuth2Bearer, req, responseBody, runReq, (/:), (=:))

data TVDBResponse = TVDBResponse
  { tvdbResponseStatus :: Text,
    tvdbResponseData :: [TVDBResponseData]
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON) via Autodocodec TVDBResponse

instance HasCodec TVDBResponse where
  codec =
    object "TVDBResponse" $
      TVDBResponse
        <$> requiredField "status" "Did this succeed?" .= tvdbResponseStatus
        <*> requiredField "data" "A list of results" .= tvdbResponseData

data TVDBResponseData = TVDBResponseData
  { tvdbResponseDataName :: Text,
    tvdbResponseDataDescription :: Maybe Text, -- overview
    tvdbResponseDataYear :: Maybe String, -- string so we can easily use `readInt` later
    tvdbResponseDataImageUrl :: Maybe Text -- image_url
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON) via Autodocodec TVDBResponseData

instance HasCodec TVDBResponseData where
  codec =
    object "TVDBResponseData" $
      TVDBResponseData
        <$> requiredField "name" "The name of this series or movie" .= tvdbResponseDataName
        <*> optionalFieldOrNull "overview" "The description" .= tvdbResponseDataDescription
        <*> optionalFieldOrNull "year" "The year when the series or movie was released" .= tvdbResponseDataYear
        <*> optionalFieldOrNull "image_url" "A link to a poster image" .= tvdbResponseDataImageUrl

data TVDBType = Series | Movie
  deriving (Generic, Show, Eq)

tryGetInfo :: BS.ByteString -> Text -> TVDBType -> Maybe Int -> IO (Maybe TVDBResponseData)
tryGetInfo tvdbToken title type' year =
  -- curl -X 'GET' \
  --   'https://api4.thetvdb.com/v4/search?query=Ghosts&type=series&limit=1' \
  --   -H 'accept: application/json'
  runReq defaultHttpConfig $ do
    r <-
      req GET (https "api4.thetvdb.com" /: "v4" /: "search") NoReqBody jsonResponse $
        mconcat
          [ "query" =: title,
            "type" =: case type' of
              Series -> "series" :: Text
              Movie -> "movie",
            "year" =: year,
            "limit" =: (1 :: Int),
            oAuth2Bearer tvdbToken
          ]
    let body :: TVDBResponse
        body = responseBody r
    case tvdbResponseStatus body of
      "success" -> do
        pure $ listToMaybe $ tvdbResponseData body
      _ -> pure Nothing
