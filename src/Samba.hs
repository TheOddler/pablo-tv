module Samba where

import Data.List (isInfixOf)
import Database.Persist.Sqlite (PersistFieldSql)
import GHC.IO.Exception (ExitCode (..))
import Path (Abs, Dir, Path, parseAbsDir)
import System.Posix (getEffectiveUserID)
import System.Process (readProcessWithExitCode)
import Yesod (FromJSON, MonadIO (liftIO), PathPiece, PersistField, ToJSON)

newtype SmbServer = SmbServer String
  deriving
    ( Eq,
      Ord,
      Show,
      Read,
      FromJSON,
      ToJSON,
      PersistField,
      PersistFieldSql,
      PathPiece
    )

newtype SmbShare = SmbShare String
  deriving
    ( Eq,
      Ord,
      Show,
      Read,
      FromJSON,
      ToJSON,
      PersistField,
      PersistFieldSql,
      PathPiece
    )

data MountResult
  = MountedSuccessfully
  | AlreadyMounted
  | FailedMounting String
  deriving (Show)

-- | This uses Gnome's gio to mount stuff. So probably won't work on anything else.
-- Maybe some day I'll figure out a way of doing this without needing Gnome. Looks like I can use `cifs-utils` for that.
mount :: (MonadIO m) => SmbServer -> SmbShare -> m MountResult
mount (SmbServer svr) (SmbShare shr) = do
  result <-
    liftIO $
      readProcessWithExitCode
        "gio"
        ["mount", "smb://" ++ svr ++ "/" ++ shr]
        ""
  pure $ case result of
    (ExitSuccess, _, _) -> MountedSuccessfully
    (_, _, err) | "Location is already mounted" `isInfixOf` err -> AlreadyMounted
    (_exitCode, _output, err) -> FailedMounting err

-- | Requires IO as it needs to find the user id
-- This assumes the location where Gnome's gio mounts stuff
mkMountPath :: (MonadIO m) => SmbServer -> SmbShare -> m (Maybe (Path Abs Dir))
mkMountPath (SmbServer svr) (SmbShare shr) = do
  uid <- liftIO getEffectiveUserID
  pure $ parseAbsDir $ "/run/user/" ++ show uid ++ "/gvfs/smb-share:server=" ++ svr ++ ",share=" ++ shr

-- uid <- getEffectiveUserID
-- let server = "192.168.0.91"
-- let share = "videos"
-- (exitCode, output, error) <- readProcessWithExitCode "gio" ["mount", "smb://" ++ server ++ "/" ++ share] ""
-- -- `catch` \(err :: SomeException) ->
-- --   putLog Error $ "gio mount failed: " ++ displayException err
-- putLog Error $ "gio mount output: " ++ show (exitCode, output, error)
-- let gvfsPath = "/run/user/" ++ show uid ++ "/gvfs/smb-share:server=" ++ server ++ ",share=" ++ share
-- mounts <- readProcess "ls" [gvfsPath] ""
-- putLog Error $ "Mounted shares: " ++ mounts
