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

runDBWithConn :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDBWithConn connPool action = runSqlPool action connPool
