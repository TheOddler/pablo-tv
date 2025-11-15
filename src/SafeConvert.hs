module SafeConvert where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
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

-- | Safe because split never returns an empty list.
-- If the predicate never matches, it returns a singleton with the input Text.
splitTextNE :: (Char -> Bool) -> T.Text -> NE.NonEmpty T.Text
splitTextNE f t = case T.split f t of
  [] -> error "Split returns a singleton with the input if the predicate doesn't match anything. So can't ever be an empty list."
  x : xs -> x NE.:| xs
