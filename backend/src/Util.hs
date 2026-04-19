module Util where

import Algorithms.NaturalSort qualified as Natural
import Control.Concurrent (MVar, takeMVar)
import Control.Exception (Exception, displayException)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.Default (def)
import Data.List (isPrefixOf, sortBy)
import Data.List.Extra (dropPrefix, unsnoc)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Ord (Down)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import GHC.Exception (errorCallException, errorCallWithCallStackException)
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import IsDevelopment (isDevelopment)
import Language.Haskell.TH.Syntax (Exp, Q, makeRelativeToProject)
import Logging (LogLevel (..), Logger, putLog)
import Network.Info (IPv4 (..), NetworkInterface (..))
import SafeIO (SafeIO (..), getCurrentTime)
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')
import Yesod
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)
import Yesod.EmbeddedStatic (embedFileAt)
import Yesod.EmbeddedStatic.Types (Generator)
import Yesod.WebSockets (race_)

ourAesonOptionsPrefix :: String -> Aeson.Options
ourAesonOptionsPrefix prefix =
  Aeson.defaultOptions
    { Aeson.unwrapUnaryRecords = True,
      Aeson.omitNothingFields = True,
      Aeson.constructorTagModifier = dropPrefix prefix,
      Aeson.fieldLabelModifier = \s -> case dropPrefix prefix s of
        "" -> ""
        f : rest -> Char.toLower f : rest
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

showT :: (Show a) => a -> T.Text
showT = T.pack . show

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

unsnocNE :: NE.NonEmpty a -> ([a], a)
unsnocNE (first NE.:| rest) = case unsnoc rest of
  Nothing -> ([], first)
  Just (inits, last') -> (first : inits, last')

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

-- | Returns the first right, and all lefts that came before it
firstRightM :: forall m a b. (Monad m) => [m (Either a b)] -> m ([a], Maybe b)
firstRightM = go []
  where
    go :: [a] -> [m (Either a b)] -> m ([a], Maybe b)
    go failures [] = pure (failures, Nothing)
    go failures (attempt : nextAttempts) = do
      result <- attempt
      case result of
        Right success -> pure (failures, Just success)
        Left failed -> go (failures ++ [failed]) nextAttempts

-- | Compares taking into account numbers properly
naturalCompareBy :: (a -> T.Text) -> a -> a -> Ordering
naturalCompareBy f a b = Natural.compare (T.toLower $ f a) (T.toLower $ f b)

-- | Sorts taking into account numbers properly
naturalSortBy :: (a -> T.Text) -> [a] -> [a]
naturalSortBy f = sortBy $ naturalCompareBy f

withDuration :: (SafeIO m) => m a -> m (a, NominalDiffTime)
withDuration f = do
  startTime <- getCurrentTime
  a <- f
  endTime <- getCurrentTime
  pure (a, diffUTCTime endTime startTime)

safeMinimumOn :: (Ord o) => (a -> o) -> [a] -> Maybe a
safeMinimumOn scale as = case NE.nonEmpty as of
  Nothing -> Nothing
  Just neA -> Just $ NE.minimumOn1 scale neA

logDuration :: (Logger m, SafeIO m) => String -> m a -> m a
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

logOnErrorIO :: (SafeIO m, Logger m) => Logging.LogLevel -> String -> IO () -> m ()
logOnErrorIO logLevel desc io = do
  resultOrErr <- runIOSafely io
  case resultOrErr of
    Right result -> pure result
    Left e -> putLog logLevel $ "Failed " ++ desc ++ ": " ++ displayException e

-- | This embeds a file but you can give it a path relative to the package root, rather than relative to where `cabal build` is run.
-- The final name you can use in the code is still the same as if you had called `Yesod.EmbeddedStatic.Generators.embedFile` with this path.
embedFileRel :: FilePath -> Generator
embedFileRel fp = do
  path <- makeRelativeToProject fp
  embedFileAt fp path
