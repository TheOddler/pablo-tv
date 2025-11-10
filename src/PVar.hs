-- | Kind of a TVar, but with some abstractions that allows for updating the
-- value with long-running IO without other threads also doing an update.
module PVar
  ( PVar,
    newPVar,
    readPVar,
    modifyPVar_,
    tryModifyPVar,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (when)

data PVarState
  = PVarReady
  | PVarUpdating
  deriving (Eq)

newtype PVar a = PVar (TVar (PVarState, a))

newPVar :: a -> IO (PVar a)
newPVar initialState = PVar <$> newTVarIO (PVarReady, initialState)

readPVar :: PVar a -> IO a
readPVar (PVar inner) = snd <$> readTVarIO inner

-- | Modify the PVar, for the duration of the IO, any other calls to modify the PVar will wait.
-- Reading the PVar is still possible though, and will return the original value immediately.
modifyPVar_ :: PVar a -> (a -> IO a) -> IO ()
modifyPVar_ (PVar inner) update = do
  value <- atomically $ do
    (state, val) <- readTVar inner
    when (state /= PVarReady) retry
    writeTVar inner (PVarUpdating, val)
    pure val
  newValue <- update value
  atomically $ writeTVar inner (PVarReady, newValue)

-- | Tries to modify the PVar, but won't if it's already being updated, rather than wait.
-- Returns whether it did an update or not.
tryModifyPVar :: PVar a -> (a -> IO a) -> IO (Maybe a)
tryModifyPVar (PVar inner) update = do
  mValue <- atomically $ do
    (state, val) <- readTVar inner
    case state of
      PVarReady -> do
        writeTVar inner (PVarUpdating, val)
        pure $ Just val
      PVarUpdating ->
        pure Nothing
  case mValue of
    Nothing -> pure Nothing
    Just value -> do
      newValue <- update value
      atomically $ writeTVar inner (PVarReady, newValue)
      pure $ Just newValue
