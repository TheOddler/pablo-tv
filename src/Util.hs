module Util where

import Control.Concurrent (MVar, takeMVar)
import Control.Exception (Exception, displayException)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.List (foldl', isPrefixOf)
import Data.List.Extra (dropPrefix)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Ord (Down)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import GHC.Exception (errorCallException, errorCallWithCallStackException)
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q)
import Logging (LogLevel (..), Logger, putLog)
import Network.Info (IPv4 (..), NetworkInterface (..))
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')
import Yesod
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)
import Yesod.WebSockets (race_)

ourAesonOptions :: Aeson.Options
ourAesonOptions =
  Aeson.defaultOptions
    { Aeson.unwrapUnaryRecords = True,
      Aeson.omitNothingFields = True
    }

ourAesonOptionsPrefix :: String -> Aeson.Options
ourAesonOptionsPrefix prefix =
  ourAesonOptions
    { Aeson.constructorTagModifier = dropPrefix prefix
    }

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

logDuration :: (Logger m, MonadIO m) => String -> m a -> m a
logDuration label action = do
  (result, duration) <- withDuration action
  putLog Info $ label ++ " (" ++ show duration ++ ")"
  pure result

impossible :: (Logger m, MonadThrow m, HasCallStack) => String -> m a
impossible msg = withFrozenCallStack $ do
  let fullMsg = "Reached the impossible: " ++ msg
  putLog Error $
    concat
      [ fullMsg,
        "\n",
        prettyCallStack callStack
      ]
  throwM $ errorCallWithCallStackException fullMsg callStack

fail404 :: (Logger m, MonadHandler m, HasCallStack) => String -> m a
fail404 msg = do
  putLog Error $
    concat
      [ msg,
        "\n",
        prettyCallStack callStack
      ]
  notFound

fail500 :: (Logger m, MonadThrow m, HasCallStack) => String -> m a
fail500 msg = do
  putLog Error $
    concat
      [ msg,
        "\n",
        prettyCallStack callStack
      ]
  throwM $ errorCallException msg

failE :: (Logger m, MonadThrow m, HasCallStack, Exception e) => String -> e -> m a
failE label e = do
  putLog Error $
    concat
      [ label,
        " failed with exception: ",
        displayException e,
        "\n",
        prettyCallStack callStack
      ]
  throwM e
