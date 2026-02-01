module WebSpec where

import Actions (mkInputDevice)
import Foundation
import GHC.Conc (newTVarIO)
import LibMain ()
import PVar (newPVar)
import TVState (startingTVState)
import Test.Syd
import Test.Syd.Yesod

mkTestApp :: IO App
mkTestApp = do
  inputDevice <- mkInputDevice
  tvState <- newTVarIO startingTVState
  lastActivePlayerTVar <- newTVarIO Nothing
  rootDirsPVar <- newPVar mempty
  pure
    App
      { appPort = 8080,
        appLogFunc = \_ _ -> pure (),
        appInputDevice = inputDevice,
        appTVState = tvState,
        appGetStatic = embeddedStatic,
        appLastActivePlayer = lastActivePlayerTVar,
        appRootDirs = rootDirsPVar
      }

spec :: Spec
spec = do
  yesodSpecWithSiteGenerator mkTestApp $ do
    xit "returns 200 on the homepage" $ do
      get HomeR
      statusIs 200
