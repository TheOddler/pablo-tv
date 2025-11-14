module SafeConvert where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | Safe because Integer is unbounded
int32ToInteger :: Int32 -> Integer
int32ToInteger = fromIntegral

-- | Safe because Text can hold all base64 characters
bsToBase64Text :: BS.ByteString -> T.Text
bsToBase64Text = TE.decodeUtf8 . B64.encode

mapTextThroughBS :: (BS.ByteString -> BS.ByteString) -> T.Text -> T.Text
mapTextThroughBS mapper = TE.decodeUtf8 . mapper . TE.encodeUtf8
