module Actions where

import Control.Monad (forever)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import System.Process (callProcess)
import Yesod (MonadHandler, liftIO)
import Yesod.WebSockets (WebSocketsT, receiveData)

data Action
  = ClickMouse
  | MoveMouse {x :: Int, y :: Int}
  | -- | Point the mouse relative to the center of the screen
    -- So (0,0) is the center of the screen
    -- Then, assuming the screen is wider then high, (0, 1) means middle top
    -- and (1, 0) means equally far to right right as the top is from the center,
    -- so something like (1.6, 0) would be center outer right
    -- Does that make sense?
    -- It's meant to be used with the mouse pointer tool
    PointMouse {leftRight :: Float, upDown :: Float}
  deriving (Generic, Show)

instance JSON.FromJSON Action

actionsWebSocket :: (MonadHandler m) => WebSocketsT m ()
actionsWebSocket = forever $ do
  d <- receiveData
  liftIO $ decodeAndPerformAction d

decodeAndPerformAction :: ByteString -> IO ()
decodeAndPerformAction websocketData = case JSON.eitherDecode websocketData of
  Left err -> putStrLn $ "Error decoding action: " <> err
  Right action -> performAction action

performAction :: Action -> IO ()
performAction = \case
  ClickMouse -> callProcess "ydotool" ["click", "0xC0"]
  MoveMouse x y -> callProcess "ydotool" ["mousemove", "-x", show x, "-y", show y]
  PointMouse lr ud -> do
    let -- I currently just hard-code the screen size because I don't have a nice way of detecting it yet
        screenHalfSize = 690 / 2 -- For some reason ydotool thinks the screen is 690 high...
        centerX = 1100 / 2 -- For some reason ydotool thinks the screen is 1100 wide?
        centerY = 690 / 2 -- For some reason ydotool thinks the screen is 690 high...
        pointerX :: Int
        pointerX = floor $ centerX + lr * screenHalfSize
        pointerY :: Int
        pointerY = floor $ centerY + ud * screenHalfSize
    callProcess
      "ydotool"
      [ "mousemove",
        "--absolute",
        "-x",
        show pointerX,
        "-y",
        show pointerY
      ]
