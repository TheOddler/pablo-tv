{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Time (UTCTime)
import Database.Persist.Sqlite (ConnectionPool, SqlBackend, runSqlPool)
import Orphanage ()
import Path (Abs, Dir, File, Path)
import System.Posix (EpochTime, FileID)
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Directory
  path (Path Abs Dir)
  lastChecked UTCTime
  fileID FileID
  modificationTime EpochTime
  accessTime EpochTime
  Primary path

VideoFile
  path (Path Abs File)
  watched UTCTime Maybe
  Primary path
|]

runDBWithConn :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDBWithConn connPool action = runSqlPool action connPool

data DirectoryInfo = DirectoryInfo
  { dirPath :: Path Abs Dir,
    dirLastModified :: EpochTime,
    dirLastAccess :: EpochTime,
    dirLastWatched :: UTCTime,
    dirFileCount :: Int,
    dirWatchedFileCount :: Int
  }
