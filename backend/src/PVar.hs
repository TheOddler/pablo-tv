-- | Kind of a TVar, but with some abstractions that allows for updating the
-- value with long-running IO without other threads also doing an update.
module PVar
  ( PVar,
    PVarState (..),
    newPVar,
    readPVar,
    readPVar',
    modifyPVar,
    modifyPVar_,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (void, when)
import Data.Text (Text)
import Data.Tuple.Extra (thd3)
import SafeIO (SafeIO (..))

data PVarState
  = PVarReady
  | PVarUpdating Text
  deriving (Eq, Show)

type Generation = Int

newtype PVar a = PVar (TVar (PVarState, Generation, a))

newPVar :: (SafeIO m) => a -> m (PVar a)
newPVar initialState = PVar <$> unsafePinkyPromiseThisIsSafe (newTVarIO (PVarReady, 0, initialState))

readPVar :: (SafeIO m) => PVar a -> m a
readPVar (PVar inner) = thd3 <$> unsafePinkyPromiseThisIsSafe (readTVarIO inner)

readPVar' :: (SafeIO m) => PVar a -> m (PVarState, Generation, a)
readPVar' (PVar inner) = unsafePinkyPromiseThisIsSafe (readTVarIO inner)

-- | Modify the PVar, for the duration of the IO, any other calls to modify the PVar will wait.
-- Reading the PVar is still possible though, and will return the original value immediately.
modifyPVar :: forall m a. (SafeIO m) => PVar a -> Text -> (a -> m a) -> m a
modifyPVar (PVar inner) desc update = do
  (generation, value) <- unsafePinkyPromiseThisIsSafe $ atomically $ do
    (state, generation, val) <- readTVar inner
    when (state /= PVarReady) retry -- Waits until ready
    writeTVar inner (PVarUpdating desc, generation, val) -- Mark as being updated, but the update we'll do outside atomically
    pure (generation, val)
  -- Update
  newValue <- update value
  -- Mark as ready for others again
  unsafePinkyPromiseThisIsSafe $ atomically $ writeTVar inner (PVarReady, generation + 1, newValue)
  pure newValue

-- | Modify but I don't care about the updated value
modifyPVar_ :: (SafeIO m) => PVar a -> Text -> (a -> m a) -> m ()
modifyPVar_ pVar desc update = do
  void $ modifyPVar pVar desc update
