{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Control.Exception (Exception (displayException), SomeException)
import Data.Text qualified as T
import Database.Persist.Sqlite (PersistFieldSql (..))
import Foreign.C (CTime (..))
import GHC.Read (Read (..))
import Path (Abs, Dir, File, Path, Rel, fromAbsDir, fromAbsFile, fromRelDir, fromRelFile, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile)
import System.Posix (CIno (..), EpochTime, FileID)
import Text.Blaze (ToMarkup (..))
import Text.Blaze qualified as Blaze
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Yesod (PathPiece (..), PersistField (..), PersistValue (..), SqlType (..))

leftExceptionToText :: Either SomeException a -> Either T.Text a
leftExceptionToText = \case
  Left e -> Left $ T.pack $ displayException e
  Right a -> Right a

instance PersistField (Path Abs Dir) where
  toPersistValue path = PersistText $ T.pack $ fromAbsDir path
  fromPersistValue (PersistText t) =
    leftExceptionToText $ parseAbsDir $ T.unpack t
  fromPersistValue x =
    Left $
      "Failed parsing Path Abs Dir: expected PersistText, received "
        <> T.pack (show x)

instance PersistFieldSql (Path Abs Dir) where
  sqlType _ = SqlString

instance PersistField (Path Abs File) where
  toPersistValue path = PersistText $ T.pack $ fromAbsFile path
  fromPersistValue (PersistText t) =
    leftExceptionToText $ parseAbsFile $ T.unpack t
  fromPersistValue x =
    Left $
      "Failed parsing Path Abs File: expected PersistText, received "
        <> T.pack (show x)

instance PersistFieldSql (Path Abs File) where
  sqlType _ = SqlString

instance PersistField (Path Rel Dir) where
  toPersistValue path = PersistText $ T.pack $ fromRelDir path
  fromPersistValue (PersistText t) =
    leftExceptionToText $ parseRelDir $ T.unpack t
  fromPersistValue x =
    Left $
      "Failed parsing Path Rel Dir: expected PersistText, received "
        <> T.pack (show x)

instance PersistFieldSql (Path Rel Dir) where
  sqlType _ = SqlString

instance PersistField (Path Rel File) where
  toPersistValue path = PersistText $ T.pack $ fromRelFile path
  fromPersistValue (PersistText t) =
    leftExceptionToText $ parseRelFile $ T.unpack t
  fromPersistValue x =
    Left $
      "Failed parsing Path Rel File: expected PersistText, received "
        <> T.pack (show x)

instance PersistFieldSql (Path Rel File) where
  sqlType _ = SqlString

instance Read (Path Abs Dir) where
  readPrec = do
    raw <- readPrec
    case parseAbsDir raw of
      Right success -> pure success
      Left err -> fail $ displayException err

instance Read (Path Abs File) where
  readPrec = do
    raw <- readPrec
    case parseAbsFile raw of
      Right success -> pure success
      Left err -> fail $ displayException err

instance Read (Path Rel Dir) where
  readPrec = do
    raw <- readPrec
    case parseRelDir raw of
      Right success -> pure success
      Left err -> fail $ displayException err

instance Read (Path Rel File) where
  readPrec = do
    raw <- readPrec
    case parseRelFile raw of
      Right success -> pure success
      Left err -> fail $ displayException err

instance FromHttpApiData (Path Abs Dir) where
  parseUrlPiece :: T.Text -> Either T.Text (Path Abs Dir)
  parseUrlPiece = leftExceptionToText . parseAbsDir . T.unpack

instance FromHttpApiData (Path Abs File) where
  parseQueryParam :: T.Text -> Either T.Text (Path Abs File)
  parseQueryParam = leftExceptionToText . parseAbsFile . T.unpack

instance ToHttpApiData (Path Abs Dir) where
  toUrlPiece :: Path Abs Dir -> T.Text
  toUrlPiece = T.pack . fromAbsDir

instance ToHttpApiData (Path Abs File) where
  toQueryParam :: Path Abs File -> T.Text
  toQueryParam = T.pack . fromAbsFile

instance PathPiece (Path Abs Dir) where
  fromPathPiece :: T.Text -> Maybe (Path Abs Dir)
  fromPathPiece = parseAbsDir . T.unpack
  toPathPiece :: Path Abs Dir -> T.Text
  toPathPiece = T.pack . fromAbsDir

instance PathPiece (Path Abs File) where
  fromPathPiece :: T.Text -> Maybe (Path Abs File)
  fromPathPiece = parseAbsFile . T.unpack
  toPathPiece :: Path Abs File -> T.Text
  toPathPiece = T.pack . fromAbsFile

instance Blaze.ToMarkup (Path Abs File) where
  toMarkup = toMarkup . fromAbsFile

deriving newtype instance PersistField EpochTime

deriving newtype instance PersistFieldSql EpochTime

deriving newtype instance PersistField FileID

deriving newtype instance PersistFieldSql FileID
