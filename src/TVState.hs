{-# LANGUAGE MultiWayIf #-}

module TVState where

import Data.Text qualified as T
import Directory (DirectoryInfo)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Path (Abs, Dir, Path, isProperPrefixOf)
import Util (onChanges)
import Watched (WatchedInfoAgg (..))
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

addToAggWatched :: TVar TVState -> Path Abs Dir -> Int -> IO ()
addToAggWatched _ _ 0 = pure ()
addToAggWatched tvStateTVar path amount = atomically $ do
  tvState <- readTVar tvStateTVar
  let updatedState = tvState {tvVideoData = doUpdate <$> tvState.tvVideoData}
  writeTVar tvStateTVar updatedState
  where
    doUpdate (p, dirIfo, watchedInfo)
      | p == path
          || p `isProperPrefixOf` path =
          ( p,
            dirIfo,
            watchedInfo
              { watchedInfoPlayedVideoFileCount =
                  watchedInfo.watchedInfoPlayedVideoFileCount + amount
              }
          )
    doUpdate x = x
