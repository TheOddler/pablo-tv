module Util where

import Autodocodec (JSONCodec, stringConstCodec)
import Control.Concurrent (MVar, takeMVar)
import Control.Monad (when)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.List (foldl', isPrefixOf)
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Ord (Down)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (Single (..))
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import GHC.List (uncons)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Info (IPv4 (..), NetworkInterface (..))
import Path (File, Path, Rel, fileExtension)
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')
import Yesod
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)
import Yesod.WebSockets (race_)

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

raceAll :: [IO ()] -> IO ()
raceAll [] = pure ()
raceAll [a] = a
raceAll (a : rest) = foldl' race_ a rest

-- | Return how likely this is the network interface people will want to connect to with their phones.
-- Mean to be used with `sortWith` so returning Down as sortWith sorts lowest first.
networkInterfaceWorthiness :: NetworkInterface -> Down Int
networkInterfaceWorthiness NetworkInterface {name, ipv4, ipv6} =
  sum
    [ hasIpv4 `as` 2,
      goodName `as` 1,
      hasIpv6 `as` 1,
      isLocalHost `as` (-10)
    ]
  where
    bool `as` val = if bool then val else 0
    goodName = any (`isPrefixOf` name) ["en", "eth", "wl"]
    hasIpv4 = ipv4 /= minBound
    hasIpv6 = ipv6 /= minBound
    isLocalHost = ipv4 == IPv4 16777343 -- Used rawShowIPv4 to find this

-- | Used to find what the internal representation of localhost is
rawShowIPv4 :: IPv4 -> String
rawShowIPv4 (IPv4 raw) = show raw

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

withDuration :: (MonadIO m) => m a -> m (a, NominalDiffTime)
withDuration f = do
  startTime <- liftIO getCurrentTime
  a <- f
  endTime <- liftIO getCurrentTime
  pure (a, diffUTCTime endTime startTime)

withKey :: (v -> k) -> v -> (k, v)
withKey keyGetter value = (keyGetter value, value)

withKeyFromValue :: (v -> k) -> Entity v -> (k, v)
withKeyFromValue keyGetter (Entity _ value) =
  (keyGetter value, value)

entityToKeyValue :: Entity a -> (Key a, a)
entityToKeyValue (Entity key val) = (key, val)

-- | Returns `posixSecondsToUTCTime 0` for empty lists.
safeMaxUTCTime :: [UTCTime] -> UTCTime
safeMaxUTCTime times = case NE.nonEmpty times of
  Nothing -> posixSecondsToUTCTime 0
  Just neTimes -> NE.maximum1 neTimes

safeMinimumOn :: (Ord o) => (a -> o) -> [a] -> Maybe a
safeMinimumOn scale as = case NE.nonEmpty as of
  Nothing -> Nothing
  Just neA -> Just $ NE.minimumOn1 scale neA

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

unSingle2 :: (Single a, Single b) -> (a, b)
unSingle2 (Single a, Single b) = (a, b)

unSingle3 :: (Single a, Single b, Single c) -> (a, b, c)
unSingle3 (Single a, Single b, Single c) = (a, b, c)

unSingle5 ::
  ( Single a,
    Single b,
    Single c,
    Single d,
    Single e
  ) ->
  (a, b, c, d, e)
unSingle5
  ( Single a,
    Single b,
    Single c,
    Single d,
    Single e
    ) = (a, b, c, d, e)

getImageContentType :: Path Rel File -> ContentType
getImageContentType filePath = case fileExtension filePath of
  Nothing -> typeOctet -- What would be the best fallback?
  Just ext ->
    let cleanedExt = lower $
          case ext of
            '.' : e -> e
            e -> e
     in "image/" <> fromString cleanedExt
