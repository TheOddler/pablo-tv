module WebSpec where

import Actions (mkInputDevice)
import Foundation
import GHC.Conc (newTVarIO)
import LibMain ()
import Logging (mkLogSlidingWindow)
import PVar (newPVar)
import TVState (startingTVState)
import Test.Syd
import Test.Syd.Yesod
import TestUtils (TestIO (..))

mkTestApp :: IO App
mkTestApp = do
  logSlidingWindow <- mkLogSlidingWindow 100
  inputDevice <- mkInputDevice
  tvState <- newTVarIO startingTVState
  lastActivePlayerTVar <- newTVarIO Nothing
  rootDirsPVar <- runTestIO $ newPVar mempty
  pure
    App
      { appPort = 8080,
        appLogFunc = \_ -> pure (),
        appLogSlidingWindow = logSlidingWindow,
        appInputDevice = inputDevice,
        appGetStatic = embeddedStatic,
        appLastActivePlayer = lastActivePlayerTVar,
        appTVState = tvState,
        appRootDirs = rootDirsPVar
      }

spec :: Spec
spec = do
  yesodSpecWithSiteGenerator mkTestApp $ do
    xit "returns 200 on the homepage" $ do
      get HomeR
      statusIs 200
