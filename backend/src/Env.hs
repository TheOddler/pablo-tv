{-# OPTIONS_GHC -Wno-orphans #-}

module Env where

import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Directory.Directories (RootDirectories)
import Evdev.Uinput (Device)
import Logging (LogFunc, LogSlidingWindow, Logger (..))
import Mpris (MediaPlayer)
import PVar (PVar)
import SafeIO (SafeIO (..))
import Servant (Handler)
import TVState (TVState)
import Transformers (SafeIOT (..))

data ServerEnv = ServerEnv
  { envPort :: Int,
    envLogFunc :: LogFunc IO,
    envLogSlidingWindow :: LogSlidingWindow,
    envInputDevice :: Device,
    envLastActivePlayer :: TVar (Maybe MediaPlayer),
    envTVState :: TVar TVState,
    envRootDirs :: PVar RootDirectories,
    envFrontend :: FilePath
  }

type ServerM = ReaderT ServerEnv Handler

instance Logger ServerM where
  putLogMsg msg = do
    logFunc <- asks envLogFunc
    liftIO $ logFunc msg

instance SafeIO Handler where
  runIOSafely = runSafeIOT . runIOSafely
  unsafePinkyPromiseThisIsSafe = runSafeIOT . unsafePinkyPromiseThisIsSafe
  getCurrentTime = runSafeIOT getCurrentTime
  getModificationTime = runSafeIOT . getModificationTime
  getHomeDirectory = runSafeIOT getHomeDirectory
  randomFileNameSuffix = runSafeIOT randomFileNameSuffix
