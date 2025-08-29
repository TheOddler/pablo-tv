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
import Database.Persist.Sqlite (ConnectionPool, SqlBackend, runSqlPool)
import Orphanage ()
import Path (Abs, Dir, File, Path, Rel)
import Yesod

type Image =
  ( -- Original file name
    Path Rel File,
    -- File bytes
    BS.ByteString
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Directory
  path (Path Abs Dir)
  image Image Maybe
  Primary path

VideoFile
  path (Path Abs File)
  added UTCTime
  watched UTCTime Maybe
  Primary path
|]

runDBWithConn :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDBWithConn connPool action = runSqlPool action connPool
