module TVState where

import Control.Monad (when)
import Data.Text (Text)
import Directory (DirectoryInfo)
import GHC.Conc (TVar)
import Path (Abs, Dir, Path)
import Util (onChanges)
import Watched (WatchedInfoAgg)
import Yesod (MonadHandler)
import Yesod.WebSockets (WebSocketsT, sendTextData)

data TVState = TVState
  { tvPage :: Text,
    tvVideoData :: [(Path Abs Dir, DirectoryInfo, WatchedInfoAgg)]
  }
  deriving (Eq)

startingTVState :: TVState
startingTVState = TVState "" []

tvStateWebSocket :: (MonadHandler m) => TVar TVState -> WebSocketsT m ()
tvStateWebSocket tvStateTVar =
  onChanges tvStateTVar $ \oldSate newState ->
    when (tvPage oldSate /= tvPage newState) $
      sendTextData $
        tvPage newState
