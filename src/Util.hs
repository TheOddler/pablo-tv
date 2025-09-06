module Util where

import Autodocodec (JSONCodec, stringConstCodec)
import Control.Concurrent (MVar, takeMVar)
import Control.Monad (when)
import Data.Default (def)
import Data.List (foldl', isPrefixOf)
import Data.List.Extra (lower)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Ord (Down)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.Persist.Sqlite (Single (..))
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Info (IPv4 (..), NetworkInterface (..))
import Path (File, Path, fileExtension)
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

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

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

safeMinimumOn :: (Ord o) => (a -> o) -> [a] -> Maybe a
safeMinimumOn scale as = case NE.nonEmpty as of
  Nothing -> Nothing
  Just neA -> Just $ NE.minimumOn1 scale neA

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

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

getImageContentType :: Path a File -> ContentType
getImageContentType filePath = case fileExtension filePath of
  Nothing -> typeOctet -- What would be the best fallback?
  Just ext ->
    let cleanedExt = lower $
          case ext of
            '.' : e -> e
            e -> e
     in "image/" <> fromString cleanedExt
