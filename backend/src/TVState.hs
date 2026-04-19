module TVState where

import Control.Monad (when)
import Data.Text qualified as T
import GHC.Conc (TVar)
import Util (onChanges)
import Yesod (MonadHandler)
import Yesod.WebSockets (WebSocketsT, sendTextData)

-- Just a newtype because it currently just has one field.
newtype TVState = TVState
  { tvPage :: T.Text
  }
  deriving (Eq)

startingTVState :: TVState
startingTVState = TVState ""

tvStateWebSocket :: (MonadHandler m) => TVar TVState -> WebSocketsT m ()
tvStateWebSocket tvStateTVar =
  onChanges tvStateTVar $ \oldSate newState ->
    when (oldSate.tvPage /= newState.tvPage) $
      sendTextData newState.tvPage
