module Util where

import Control.Concurrent.Async (race_)
import Control.Exception (displayException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List.Extra (unsnoc)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NE
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, retry)
import Logging (LogLevel (..), Logger, putLog)
import SafeIO (SafeIO (..), getCurrentTime)

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

showT :: (Show a) => a -> T.Text
showT = T.pack . show

unsnocNE :: NE.NonEmpty a -> ([a], a)
unsnocNE (first NE.:| rest) = case unsnoc rest of
  Nothing -> ([], first)
  Just (inits, last') -> (first : inits, last')

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

logOnErrorIO :: (SafeIO m, Logger m) => Logging.LogLevel -> String -> IO () -> m ()
logOnErrorIO logLevel desc io = do
  resultOrErr <- runIOSafely io
  case resultOrErr of
    Right result -> pure result
    Left e -> putLog logLevel $ "Failed " ++ desc ++ ": " ++ displayException e
