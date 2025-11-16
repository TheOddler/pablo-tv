-- | Kind of a TVar, but with some abstractions that allows for updating the
-- value with long-running IO without other threads also doing an update.
module PVar
  ( PVar,
    newPVar,
    readPVar,
    modifyPVar,
    modifyPVar_,
    tryModifyPVar,
    tryModifyPVar_,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (void, when)
import Yesod (MonadIO (..))

data PVarState
  = PVarReady
  | PVarUpdating
  deriving (Eq)

newtype PVar a = PVar (TVar (PVarState, a))

newPVar :: (MonadIO m) => a -> m (PVar a)
newPVar initialState = PVar <$> liftIO (newTVarIO (PVarReady, initialState))

readPVar :: (MonadIO m) => PVar a -> m a
readPVar (PVar inner) = snd <$> liftIO (readTVarIO inner)

-- | Modify the PVar, for the duration of the IO, any other calls to modify the PVar will wait.
-- Reading the PVar is still possible though, and will return the original value immediately.
modifyPVar :: forall m a. (MonadIO m) => PVar a -> (a -> m a) -> m a
modifyPVar (PVar inner) update = do
  value <- liftIO $ atomically $ do
    (state, val) <- readTVar inner
    when (state /= PVarReady) retry -- Waits until ready
    writeTVar inner (PVarUpdating, val) -- Mark as being updated, but the update we'll do outside atomically
    pure val
  newValue <- update value
  liftIO $ atomically $ writeTVar inner (PVarReady, newValue) -- Mark as ready for others again
  pure newValue

-- | Modify but I don't care about the updated value
modifyPVar_ :: (MonadIO m) => PVar a -> (a -> m a) -> m ()
modifyPVar_ pVar update = do
  void $ modifyPVar pVar update

-- | Tries to modify the PVar but, rather than wait, won't if it's already being updated.
-- Returns whether it did an update or not.
tryModifyPVar :: (MonadIO m) => PVar a -> (a -> m a) -> m (Maybe a)
tryModifyPVar (PVar inner) update = do
  mValue <- liftIO $ atomically $ do
    (state, val) <- readTVar inner
    case state of
      PVarReady -> do
        writeTVar inner (PVarUpdating, val) -- We will update, so mark as being updated
        pure $ Just val
      PVarUpdating ->
        -- Someone else is already updating, so we do nothing
        pure Nothing
  case mValue of
    Nothing -> pure Nothing
    Just value -> do
      newValue <- update value
      liftIO $ atomically $ writeTVar inner (PVarReady, newValue)
      pure $ Just newValue

tryModifyPVar_ :: (MonadIO m) => PVar a -> (a -> m a) -> m ()
tryModifyPVar_ pVar update =
  void $ tryModifyPVar pVar update
