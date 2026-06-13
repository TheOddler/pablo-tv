{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module NetworkInfo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Env (ServerEnv (..), ServerM)
import JSON (HasJSONPrefix (..), deriveToJSONPrefixed)
import Network.Info (NetworkInterface (..), getNetworkInterfaces)

data SimpleNetworkInterface = SimpleNetworkInterface
  { sniName :: String,
    sniIpv4 :: String
  }

instance HasJSONPrefix SimpleNetworkInterface where
  type JSONPrefix SimpleNetworkInterface = "sni"

data NetworkInfo = NetworkInfo
  { networkPort :: Int,
    networkInterfaces :: [SimpleNetworkInterface]
  }

instance HasJSONPrefix NetworkInfo where
  type JSONPrefix NetworkInfo = "network"

getNetworkInfo :: ServerM NetworkInfo
getNetworkInfo = do
  port <- asks envPort
  networkInterfaces' <- liftIO getNetworkInterfaces

  pure $
    NetworkInfo
      port
      [ SimpleNetworkInterface
          { sniName = i.name,
            sniIpv4 = show i.ipv4
          }
      | i <- networkInterfaces'
      ]

deriveToJSONPrefixed ''SimpleNetworkInterface
deriveToJSONPrefixed ''NetworkInfo
