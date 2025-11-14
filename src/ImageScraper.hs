module ImageScraper where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Either.Extra (mapLeft)
import Data.List.Extra (replace)
import Data.Text qualified as T
import Network.HTTP.Client (Response (..))
import Network.HTTP.Req
import Network.HTTP.Req qualified as HTTP
import Network.HTTP.Types (urlEncode)
import SafeConvert (mapTextThroughBS)
import Text.HTML.Scalpel
import Text.URI (mkURI)
import UnliftIO.Exception (tryAny)
import Yesod (ContentType)

data ImageSearchFailure
  = ImageSearchFailedScraping
  | ImageSearchDownloadFailed String
  deriving (Show)

tryFindImage :: (MonadIO m) => T.Text -> m (Either ImageSearchFailure (ContentType, BS.ByteString))
tryFindImage searchText = do
  mImgUrl <- liftIO $ scrapeURL (T.unpack url) firstImageUrl
  case mImgUrl of
    Nothing -> pure $ Left ImageSearchFailedScraping
    Just imgUrl -> do
      let resizedUrl = mkImgSizeUrl imgUrl
      imgOrErr <- downloadImage resizedUrl
      case imgOrErr of
        Left err -> pure $ Left $ ImageSearchDownloadFailed err
        Right img -> pure $ Right img
  where
    baseUrl = "https://www.themoviedb.org/search?query="
    query = mapTextThroughBS (urlEncode True) searchText
    url = baseUrl <> query

    firstImageUrl :: Scraper String URL
    firstImageUrl = chroot ("div" @: [hasClass "poster"]) innerImage

    innerImage :: Scraper String URL
    innerImage = attr "src" $ "img" @: [hasClass "poster"]

    -- Looks like the thumbnail urls have a simple pattern, so we can get a bigger size.
    -- Format for the urls is: https://media.themoviedb.org/t/p/w94_and_h141_bestv2/123SomeHash456.jpg
    -- TODO: Do we need to make this more flexible with some regex? Maybe not all thumbnails are 91 by 141
    mkImgSizeUrl = replace "w94_and_h141" "w300_and_h450"

downloadImage :: (MonadIO m) => URL -> m (Either String (ContentType, BS.ByteString))
downloadImage imageUrl = do
  case useURI =<< mkURI (T.pack imageUrl) of
    Nothing -> pure $ Left $ "Invalid URL: " ++ imageUrl
    Just (Left httpUrl) -> downloadFrom httpUrl
    Just (Right httpsUrl) -> downloadFrom httpsUrl
  where
    downloadFrom :: (MonadIO m) => (Url scheme, Option scheme) -> m (Either String (ContentType, BS.ByteString))
    downloadFrom (url, options) = do
      imgResponse <-
        liftIO . tryAny . HTTP.runReq defaultHttpConfig $
          req GET url NoReqBody bsResponse options
      pure $ do
        img <- mapLeft (\err -> "Failed downloading image: " <> displayException err) imgResponse
        let headers = responseHeaders $ toVanillaResponse img
        contentType <- lookupHeader "Content-Type" headers
        pure (contentType, HTTP.responseBody img)

    lookupHeader :: (Eq a, Show a, Show b) => a -> [(a, b)] -> Either String b
    lookupHeader x xs = case lookup x xs of
      Just y -> Right y
      Nothing -> Left $ "Failed to find " <> show x <> " in headers: " <> show xs
