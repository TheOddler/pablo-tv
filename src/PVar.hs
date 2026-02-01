-- | Kind of a TVar, but with some abstractions that allows for updating the
-- value with long-running IO without other threads also doing an update.
module PVar
  ( PVar,
    PVarState (..),
    newPVar,
    readPVar,
    readPVarState,
    modifyPVar,
    modifyPVar_,
    tryModifyPVar,
    tryModifyPVar_,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (void, when)
import Data.Text (Text)
import Yesod (MonadIO (..))

data PVarState
  = PVarReady
  | PVarUpdating Text
  deriving (Eq)

newtype PVar a = PVar (TVar (PVarState, a))

newPVar :: (MonadIO m) => a -> m (PVar a)
newPVar initialState = PVar <$> liftIO (newTVarIO (PVarReady, initialState))

readPVar :: (MonadIO m) => PVar a -> m a
readPVar (PVar inner) = snd <$> liftIO (readTVarIO inner)

readPVarState :: (MonadIO m) => PVar a -> m PVarState
readPVarState (PVar inner) = fst <$> liftIO (readTVarIO inner)

-- | Modify the PVar, for the duration of the IO, any other calls to modify the PVar will wait.
-- Reading the PVar is still possible though, and will return the original value immediately.
modifyPVar :: forall m a. (MonadIO m) => PVar a -> Text -> (a -> m a) -> m a
modifyPVar (PVar inner) desc update = do
  value <- liftIO $ atomically $ do
    (state, val) <- readTVar inner
    when (state /= PVarReady) retry -- Waits until ready
    writeTVar inner (PVarUpdating desc, val) -- Mark as being updated, but the update we'll do outside atomically
    pure val
  newValue <- update value
  liftIO $ atomically $ writeTVar inner (PVarReady, newValue) -- Mark as ready for others again
  pure newValue

-- | Modify but I don't care about the updated value
modifyPVar_ :: (MonadIO m) => PVar a -> Text -> (a -> m a) -> m ()
modifyPVar_ pVar desc update = do
  void $ modifyPVar pVar desc update
