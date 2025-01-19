-- | The idea here is to experiment with not exposing all of IO in a lot of
-- places, and break it up in I, O and network operations.
-- That way I'm hoping to crease easier to reason about code, and make it easier
-- to separate concerns.
module SaferIO where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS8
import GHC.Data.Maybe (rightToMaybe)
import Network.HTTP.Req qualified as HTTP
import Path (Abs, Dir, File, Path, Rel)
import Path qualified
import Path.IO qualified as Path

-- | Can read from the file system
class (Monad m) => FSRead m where
  getHomeDir :: m (Path Abs Dir)
  listDirRel :: Path Abs Dir -> m ([Path Rel Dir], [Path Rel File])
  readFileBSSafe :: Path Abs File -> m (Maybe BS8.ByteString)

instance FSRead IO where
  getHomeDir = Path.getHomeDir
  listDirRel = Path.listDirRel
  readFileBSSafe path = do
    (contentOrErr :: Either SomeException BS8.ByteString) <-
      try (BS8.readFile $ Path.fromAbsFile path)
    pure $ rightToMaybe contentOrErr

class (Monad m) => FSWrite m where
  writeFileBS :: Path Abs File -> BS8.ByteString -> m ()
  renameFileSafe :: Path Abs File -> Path Abs File -> m ()

instance FSWrite IO where
  writeFileBS path = BS8.writeFile (Path.fromAbsFile path)
  renameFileSafe a b = do
    (_ :: Either SomeException ()) <-
      try $ Path.renameFile a b
    pure ()

class (Monad m) => NetworkRead m where
  runReqSafe :: HTTP.HttpConfig -> HTTP.Req a -> m (Either HTTP.HttpException a)

instance NetworkRead IO where
  runReqSafe c = try . HTTP.runReq c

class (Monad m) => Logger m where
  logStr :: String -> m ()

instance Logger IO where
  logStr = putStrLn