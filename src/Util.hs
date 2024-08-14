module Util where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (def)
import Data.List (isPrefixOf)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Info (NetworkInterface (..))
import Text.Julius qualified as Julius
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

-- | Load a widget file, automatically reloading it in development.
widgetFile :: String -> Q Exp
widgetFile =
  if isDevelopment
    then widgetFileReload settings
    else widgetFileNoReload settings
  where
    settings = def

-- | Load a julius file, automatically reloading it in development.
juliusFile :: String -> Q Exp
juliusFile =
  if isDevelopment
    then Julius.juliusFile
    else Julius.juliusFileReload

-- | Run an action whenever the value of the 'TVar' changes.
onChanges :: (Eq a, MonadIO m) => TVar a -> (a -> m ()) -> m b
onChanges tVar f = do
  a <- liftIO $ readTVarIO tVar
  f a
  loop a
  where
    loop a = do
      a' <- liftIO $ atomically $ do
        a' <- readTVar tVar
        -- If value hasn't changed, retry, which will block until the value changes
        when (a == a') retry
        pure a'
      f a'
      loop a'

-- | Get a list of network interfaces that most likely have the IP people will want to connect to with their phones
networkInterfacesShortList :: [NetworkInterface] -> [NetworkInterface]
networkInterfacesShortList = filter onShortList
  where
    onShortList :: NetworkInterface -> Bool
    onShortList NetworkInterface {name} = any (`isPrefixOf` name) ["en", "eth", "wl"]
