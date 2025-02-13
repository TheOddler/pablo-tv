{-# LANGUAGE MultiWayIf #-}

module TVState where

import Data.Text qualified as T
import Directory (DirectoryInfo)
import GHC.Conc (TVar)
import Path (Abs, Dir, Path)
import Util (onChanges)
import Watched (WatchedInfoAgg)
import Yesod (MonadHandler)
import Yesod.WebSockets (WebSocketsT, sendTextData)

data TVState = TVState
  { tvPage :: T.Text,
    tvVideoData :: [(Path Abs Dir, DirectoryInfo, WatchedInfoAgg)]
  }
  deriving (Eq)

startingTVState :: TVState
startingTVState = TVState "" []

tvStateWebSocket :: (MonadHandler m) => TVar TVState -> WebSocketsT m ()
tvStateWebSocket tvStateTVar =
  onChanges tvStateTVar $ \oldSate newState ->
    if
      | oldSate.tvPage /= newState.tvPage ->
          sendTextData newState.tvPage
      | ignoreWatchedInfo oldSate /= ignoreWatchedInfo newState ->
          sendTextData $ T.pack "refresh"
      | otherwise -> pure ()
  where
    ignoreWatchedInfo tvSate = drop3rd <$> tvSate.tvVideoData
    drop3rd (a, b, _) = (a, b)
