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
import Network.Info (NetworkInterface (..))
import Text.Julius qualified as Julius
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

-- | Convert a route to a URL for the current website
toUrl :: (MonadHandler m, Yesod (HandlerSite m), RenderRoute a) => Route a -> m LBS.ByteString
toUrl route = do
  site <- getYesod
  request <- waiRequest
  let (segments, parameters) = renderRoute route
  let root = getApprootText approot site request
  pure $ toLazyByteString $ joinPath site root segments parameters

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
