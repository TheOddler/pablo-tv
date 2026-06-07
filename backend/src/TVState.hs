module TVState where

import Data.Text qualified as T

-- Just a newtype because it currently just has one field.
newtype TVState = TVState
  { tvPage :: T.Text
  }
  deriving (Eq)

startingTVState :: TVState
startingTVState = TVState ""

-- tvStateWebSocket :: (MonadHandler m) => TVar TVState -> WebSocketsT m ()
-- tvStateWebSocket tvStateTVar =
--   onChanges tvStateTVar $ \oldSate newState ->
--     when (oldSate.tvPage /= newState.tvPage) $
--       sendTextData newState.tvPage
