module Actions where

import Control.Monad (forever)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32)
import Evdev.Codes
import Evdev.Uinput
import GHC.Generics (Generic)
import System.Process (callProcess)
import Yesod (MonadHandler, liftIO)
import Yesod.WebSockets (WebSocketsT, receiveData)

data Action
  = ClickMouse
  | MoveMouse {x :: Int32, y :: Int32}
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

actionsWebSocket :: (MonadHandler m) => Device -> WebSocketsT m ()
actionsWebSocket mouse = forever $ do
  d <- receiveData
  liftIO $ decodeAndPerformAction mouse d

decodeAndPerformAction :: Device -> ByteString -> IO ()
decodeAndPerformAction mouse websocketData = case JSON.eitherDecode websocketData of
  Left err -> putStrLn $ "Error decoding action: " <> err
  Right action -> performAction mouse action

performAction :: Device -> Action -> IO ()
performAction mouse = \case
  ClickMouse -> callProcess "ydotool" ["click", "0xC0"]
  MoveMouse x y -> do
    writeBatch
      mouse
      [ RelativeEvent RelX $ EventValue x,
        RelativeEvent RelY $ EventValue y
      ]
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

mkVirtualMouse :: IO Device
mkVirtualMouse =
  newDevice
    "Virtual Mouse"
    DeviceOpts
      { phys = Nothing,
        uniq = Nothing,
        idProduct = Nothing,
        idVendor = Nothing,
        idBustype = Nothing,
        idVersion = Nothing,
        keys = [BtnLeft, BtnRight, BtnMiddle],
        relAxes = [RelX, RelY],
        absAxes =
          [ ( AbsX,
              AbsInfo
                { absValue = 0,
                  absMinimum = 0,
                  absMaximum = 1920,
                  absFuzz = 0,
                  absFlat = 0,
                  absResolution = 0
                }
            ),
            ( AbsY,
              AbsInfo
                { absValue = 0,
                  absMinimum = 0,
                  absMaximum = 1080,
                  absFuzz = 0,
                  absFlat = 0,
                  absResolution = 0
                }
            )
          ],
        miscs = [],
        switchs = [],
        leds = [],
        sounds = [], -- Maybe SndClick?
        reps = [],
        ffs = [],
        powers = [],
        ffStats = []
      }
