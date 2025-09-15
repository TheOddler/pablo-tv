{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB where

import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString qualified as BS
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql.Raw.QQ (sqlQQ)
import Database.Persist.Sqlite (ConnectionPool, Single (..), SqlBackend, runSqlPool)
import Orphanage ()
import Path (Abs, Dir, File, Path, Rel)
import Samba (SmbServer, SmbShare)
import Util (unSingle5, uncurry5)
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
SambaShare
  smbServer SmbServer
  smbShare SmbShare
  Primary smbServer smbShare
  
Directory
  path (Path Abs Dir)
  -- Image and ImageContentType should be kept in sync, either both Nothing or Both Just. But we save them separately because if we save them as a tuple together the image bytes get turned into a varchar
  imageContentType ContentType Maybe
  image BS.ByteString Maybe
  Primary path

VideoFile
  parent DirectoryId OnDeleteCascade OnUpdateCascade
  name (Path Rel File)
  added UTCTime
  watched UTCTime Maybe
  Primary parent name
|]

runDBPool :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDBPool connPool action = runSqlPool action connPool

-- | This returns ll directories that are not the child of any other directory in the DB.
-- Is this a good way? Or should I road the samba shares in the DB?
-- Though there might be other local folders, so maybe not.
-- Not sure what is the best source of truth.
getAllRootDirectories ::
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m [Path Abs Dir]
getAllRootDirectories =
  map unSingle
    <$> [sqlQQ|
      SELECT d.@{DirectoryPath} -- This is unique as the path is the primary key
      FROM ^{Directory} d
      WHERE NOT EXISTS (
        SELECT 1
        FROM ^{Directory} other
        WHERE d.@{DirectoryPath} <> other.@{DirectoryPath}
        AND d.@{DirectoryPath} GLOB other.@{DirectoryPath} || '*'
      )
    |]

data AggDirInfo = AggDirInfo
  { aggDirPath :: Path Abs Dir,
    aggDirLastModified :: UTCTime,
    aggDirLastWatched :: UTCTime,
    aggDirVideoFileCount :: Int,
    aggDirPlayedVideoFileCount :: Int
  }

getAggSubDirsInfoQ ::
  (MonadUnliftIO m) =>
  Path Abs Dir ->
  ReaderT SqlBackend m [AggDirInfo]
getAggSubDirsInfoQ root = do
  let epoch = posixSecondsToUTCTime 0
  raw <-
    [sqlQQ|
      SELECT
        d.@{DirectoryPath},
        COALESCE(max(v.@{VideoFileAdded}), #{epoch}), -- Added can be `NULL` if there are no video files
        COALESCE(max(v.@{VideoFileWatched}), #{epoch}),
        count(v.@{VideoFileName}),
        SUM(IIF(v.@{VideoFileWatched} IS NOT NULL, 1, 0))
      FROM ^{Directory} d
      LEFT JOIN ^{VideoFile} v
        -- Join all video files that are direct or indirect children
        ON v.@{VideoFileParent} GLOB d.@{DirectoryPath} || '*'
      WHERE 
        -- This checks that it's a sub-directory
        -- SQLite has some GLOB optimisations, so this is fast
        d.@{DirectoryPath} GLOB #{root} || '*'
        -- This makes sure it's a direct child
        AND instr(rtrim(substr(d.@{DirectoryPath}, length(#{root})+1), '/'), '/') = 0
        -- And this removed the root itself
        AND d.@{DirectoryPath} <> #{root}
      GROUP BY
        d.@{DirectoryPath}
    |]
  pure $ uncurry5 AggDirInfo . unSingle5 <$> raw

getImageQ ::
  (MonadUnliftIO m) =>
  Path Abs Dir ->
  ReaderT SqlBackend m (Maybe (ContentType, BS.ByteString))
getImageQ root = do
  raw <-
    [sqlQQ|
      SELECT @{DirectoryImageContentType}, @{DirectoryImage}
      FROM ^{Directory}
      WHERE @{DirectoryImageContentType} IS NOT NULL
      AND @{DirectoryImage} IS NOT NULL
      -- Any image that is in the given path, or any of it's parents
      AND #{root} GLOB @{DirectoryPath} || '*'
      ORDER BY abs(length(@{DirectoryPath}) - length(#{root}))
      LIMIT 1
    |]
  pure $ case raw of
    [] -> Nothing
    (Single imgName, Single imgBytes) : _ -> Just (imgName, imgBytes)

hasImageQ ::
  (MonadUnliftIO m) =>
  Path Abs Dir ->
  ReaderT SqlBackend m Bool
hasImageQ root = do
  -- Should return wether or not the `getNearestImage` will return an image.
  raw <-
    [sqlQQ|
      SELECT 1
      FROM ^{Directory}
      WHERE @{DirectoryImageContentType} IS NOT NULL
      AND @{DirectoryImage} IS NOT NULL
      -- Any image that is in the given path, or any of it's parents
      AND #{root} GLOB @{DirectoryPath} || '*'
      LIMIT 1
    |]
  pure $ case raw of
    [] -> False
    (Single i) : _ -> i > (0 :: Int)
