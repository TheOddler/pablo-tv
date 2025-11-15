{-# LANGUAGE DerivingStrategies #-}

module Util.TextWithoutSeparator
  ( TextWithoutSeparator,
    Unwrap (..),
    textWithoutSeparator,
    unTextWithoutSeparator,
    removeSeparatorsFromText,
    splitAtSeparator,
    splitAtSeparatorNE,
    unsplitSeparatedText,
    unsplitSeparatedTextNE,
  )
where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import SafeConvert (splitTextNE)
import Text.Blaze (ToMarkup)
import Text.Read (Read (..))
import Yesod (PathPiece (..))

-- | A class to unwrap nested newtypes to their inner type
class Unwrap inner a where
  unwrap :: a -> inner

-- Text without separator, stuff like directory names, file names, ...
-- Safe to use as path piece.
separator :: Char
separator = '/'

newtype TextWithoutSeparator = UnsafeTextWithoutSeparator {unTextWithoutSeparator :: T.Text}
  deriving newtype (Eq, Ord, Show, ToJSON, ToJSONKey, ToMarkup)

instance Read TextWithoutSeparator where
  readPrec = readPrec >>= textWithoutSeparator

instance FromJSON TextWithoutSeparator where
  parseJSON v = parseJSON v >>= textWithoutSeparator

instance FromJSONKey TextWithoutSeparator where
  fromJSONKey = FromJSONKeyTextParser textWithoutSeparator

instance PathPiece TextWithoutSeparator where
  toPathPiece :: TextWithoutSeparator -> T.Text
  toPathPiece = unTextWithoutSeparator

  fromPathPiece :: T.Text -> Maybe TextWithoutSeparator
  fromPathPiece = textWithoutSeparator

instance Unwrap T.Text TextWithoutSeparator where
  unwrap = unTextWithoutSeparator

textWithoutSeparator :: (MonadFail m) => T.Text -> m TextWithoutSeparator
textWithoutSeparator t =
  if separator `T.elem` t
    then fail $ "Text contains a " ++ [separator] ++ " but shouldn't."
    else pure $ UnsafeTextWithoutSeparator t

removeSeparatorsFromText :: T.Text -> TextWithoutSeparator
removeSeparatorsFromText = UnsafeTextWithoutSeparator . T.filter (/= separator)

splitAtSeparator :: T.Text -> [TextWithoutSeparator]
splitAtSeparator = NE.toList . splitAtSeparatorNE

splitAtSeparatorNE :: T.Text -> NE.NonEmpty TextWithoutSeparator
splitAtSeparatorNE t = UnsafeTextWithoutSeparator <$> splitTextNE (== separator) t

unsplitSeparatedText :: [TextWithoutSeparator] -> T.Text
unsplitSeparatedText = T.intercalate (T.singleton separator) . map unTextWithoutSeparator

unsplitSeparatedTextNE :: NE.NonEmpty TextWithoutSeparator -> T.Text
unsplitSeparatedTextNE = unsplitSeparatedText . NE.toList
