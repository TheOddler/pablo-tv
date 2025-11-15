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
    safeCleanListDirectory,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Utils.Exception (tryIO)
import SafeConvert (splitTextNE)
import System.Directory (listDirectory)
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

-- | A safer version of `listDirectory` that also returns a cleaner result that
-- encodes the fact there are no separators in the paths in the type.
safeCleanListDirectory :: (MonadIO m) => FilePath -> m (Either IOError [TextWithoutSeparator])
safeCleanListDirectory dirPath = do
  namesOrErr <- liftIO $ tryIO $ listDirectory dirPath
  pure $ case namesOrErr of
    Left err -> Left err
    Right names ->
      -- We know the listed paths won't have separators in them, so safe to do
      Right $ UnsafeTextWithoutSeparator . T.pack <$> names
