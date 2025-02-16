module Util where

import Autodocodec (JSONCodec, stringConstCodec)
import Control.Concurrent (MVar, takeMVar)
import Control.Monad (when)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import GHC.List (uncons)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Info (IPv4 (..), NetworkInterface (..))
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')
import Yesod
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

-- | Load a widget file, automatically reloading it in development.
widgetFile :: String -> Q Exp
widgetFile =
  if isDevelopment
    then widgetFileReload settings
    else widgetFileNoReload settings
  where
    settings = def

-- | Run an action whenever the value of the 'TVar' changes.
onChanges :: (Eq a, MonadIO m) => TVar a -> (a -> a -> m ()) -> m b
onChanges tVar f = do
  a <- liftIO $ readTVarIO tVar
  loop a
  where
    loop prevVal = do
      newVal <- liftIO $ atomically $ do
        newVal <- readTVar tVar
        -- If value hasn't changed, retry, which will block until the value changes
        when (prevVal == newVal) retry
        pure newVal
      f prevVal newVal
      loop newVal

-- | Return how likely this is the network interface people will want to connect to with their phones.
-- Mean to be used with `sortWith` so smaller numbers is more worthy.
networkInterfaceWorthiness :: NetworkInterface -> Int
networkInterfaceWorthiness NetworkInterface {name, ipv4, ipv6} =
  8
    - goodName `as` 4
    + hasIpv4 `as` 2
    + hasIpv6 `as` 1
  where
    bool `as` val = if bool then val else 0
    goodName = any (`isPrefixOf` name) ["en", "eth", "wl"]
    hasIpv4 = ipv4 /= minBound
    hasIpv6 = ipv6 /= minBound

showIpV4OrV6WithPort :: Int -> NetworkInterface -> [Char]
showIpV4OrV6WithPort port i =
  ( if ipv4 i == IPv4 0
      then show $ ipv6 i
      else show $ ipv4 i
  )
    ++ ":"
    ++ show port

-- | Convert a route to a URL for the current website
toUrlRel :: (MonadHandler m, Yesod (HandlerSite m), RenderRoute a) => Route a -> m LBS.ByteString
toUrlRel route = do
  site <- getYesod
  let (segments, parameters) = renderRoute route
  pure $ toLazyByteString $ joinPath site "" segments parameters

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

-- | Not yet available in the base I use, but should be replaceable in a later version
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

removeLast :: [a] -> Maybe [a]
removeLast arr = case reverse arr of
  [] -> Nothing
  _ : xs -> Just $ reverse xs

boundedEnumCodec ::
  forall enum.
  (Eq enum, Enum enum, Bounded enum) =>
  (enum -> T.Text) ->
  JSONCodec enum
boundedEnumCodec showFunc =
  let ls = [minBound .. maxBound]
   in case NE.nonEmpty ls of
        Nothing -> error "0 enum values ?!"
        Just ne -> stringConstCodec (NE.map (\v -> (v, showFunc v)) ne)

-- | This will loop and take the `()` from the trigger, and rerun the action
-- every time the trigger is set.
-- If the trigger is set while the action is already running, it will wait until
-- it is done and then run the action.
asyncOnTrigger :: MVar () -> IO () -> IO ()
asyncOnTrigger trigger action = loop
  where
    loop :: IO ()
    loop = do
      takeMVar trigger
      action
      loop

shuffle :: (RandomGen gen) => [a] -> gen -> [a]
shuffle [] _ = []
shuffle list gen = shuffle' list (length list) gen
