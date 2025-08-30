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
import Util (unSingle5, uncurry5)
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Directory
  path (Path Abs Dir)
  -- Image and ImageName should be kept in sync, either both Nothing or Both Just. But we save them separately because if we save them as a tuple together the image bytes get turned into a varchar
  imageName (Path Rel File) Maybe
  image BS.ByteString Maybe
  Primary path

VideoFile
  parent DirectoryId
  name (Path Rel File)
  added UTCTime
  watched UTCTime Maybe
  Primary parent name
|]

runDBPool :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDBPool connPool action = runSqlPool action connPool

data AggDirInfo = AggDirInfo
  { aggDirPath :: Path Abs Dir,
    aggDirLastModified :: UTCTime,
    aggDirLastWatched :: UTCTime,
    aggDirVideoFileCount :: Int,
    aggDirPlayedVideoFileCount :: Int
  }

getAggSubDirInfoQ ::
  (MonadUnliftIO m) =>
  Path Abs Dir ->
  ReaderT SqlBackend m [AggDirInfo]
getAggSubDirInfoQ root = do
  let epoch = posixSecondsToUTCTime 0
  raw <-
    [sqlQQ|
      SELECT
        d.@{DirectoryPath},
        max(v.@{VideoFileAdded}),
        COALESCE(max(v.@{VideoFileWatched}), #{epoch}),
        count(*),
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

getNearestImage ::
  (MonadUnliftIO m) =>
  Path Abs Dir ->
  ReaderT SqlBackend m (Maybe (Path Rel File, BS.ByteString))
getNearestImage root = do
  raw <-
    [sqlQQ|
      SELECT @{DirectoryImageName}, @{DirectoryImage}
      FROM ^{Directory}
      WHERE
          @{DirectoryImageName} IS NOT NULL
      AND @{DirectoryImage} IS NOT NULL
      AND (
        -- Any image that is in the given path, or any of it's parents
        #{root} GLOB @{DirectoryPath} || '*'
        -- Or any image in a child folder
        OR @{DirectoryPath} GLOB #{root} || '*'
      )
      ORDER BY abs(length(@{DirectoryPath}) - length(#{root}))
      LIMIT 1
    |]
  pure $ case raw of
    [] -> Nothing
    (Single imgName, Single imgBytes) : _ -> Just (imgName, imgBytes)
