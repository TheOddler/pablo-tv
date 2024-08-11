module Actions where

import Control.Monad (forever)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32)
import Data.List (nub)
import Evdev.Codes
import Evdev.Uinput
import GHC.Float (int2Float)
import GHC.Generics (Generic)
import SafeMaths (int32ToInt)
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
  | Write {text :: String}
  deriving (Generic, Show)

instance JSON.FromJSON Action

actionsWebSocket :: (MonadHandler m) => Device -> WebSocketsT m ()
actionsWebSocket inputDevice = forever $ do
  d <- receiveData
  liftIO $ decodeAndPerformAction inputDevice d

decodeAndPerformAction :: Device -> ByteString -> IO ()
decodeAndPerformAction inputDevice websocketData = case JSON.eitherDecode websocketData of
  Left err -> putStrLn $ "Error decoding action: " <> err
  Right action -> performAction inputDevice action

performAction :: Device -> Action -> IO ()
performAction inputDevice = \case
  ClickMouse ->
    writeBatch
      inputDevice
      [ KeyEvent BtnLeft Pressed,
        SyncEvent SynReport,
        KeyEvent BtnLeft Released
        -- Batch automatically adds a sync at the end too
      ]
  MoveMouse x y ->
    writeBatch
      inputDevice
      [ RelativeEvent RelX $ EventValue x,
        RelativeEvent RelY $ EventValue y
      ]
  PointMouse lr ud -> do
    let -- I assume the screen is in landscape mode
        int32ToFloat = int2Float . int32ToInt
        screenHalfSize = int32ToFloat screenHeight / 2
        centerX = int32ToFloat screenWidth / 2
        centerY = int32ToFloat screenHeight / 2
        pointerX = floor $ centerX + lr * screenHalfSize
        pointerY = floor $ centerY + ud * screenHalfSize
    writeBatch
      inputDevice
      [ AbsoluteEvent AbsX $ EventValue pointerX,
        AbsoluteEvent AbsY $ EventValue pointerY
      ]
  Write text -> do
    let clickKeyCombo keys =
          map (`KeyEvent` Pressed) keys
            ++ [SyncEvent SynReport]
            ++ map (`KeyEvent` Released) (reverse keys)
            ++ [SyncEvent SynReport]
        events = concatMap (clickKeyCombo . charToKey) text
    putStrLn $ "Writing " ++ text
    putStrLn $ "Events: " ++ show events
    writeBatch inputDevice events

charToKey :: Char -> [Key]
charToKey = \case
  ' ' -> [KeySpace]
  'a' -> [KeyA]
  'b' -> [KeyB]
  'c' -> [KeyC]
  'd' -> [KeyD]
  'e' -> [KeyE]
  'f' -> [KeyF]
  'g' -> [KeyG]
  'h' -> [KeyH]
  'i' -> [KeyI]
  'j' -> [KeyJ]
  'k' -> [KeyK]
  'l' -> [KeyL]
  'm' -> [KeyM]
  'n' -> [KeyN]
  'o' -> [KeyO]
  'p' -> [KeyP]
  'q' -> [KeyQ]
  'r' -> [KeyR]
  's' -> [KeyS]
  't' -> [KeyT]
  'u' -> [KeyU]
  'v' -> [KeyV]
  'w' -> [KeyW]
  'x' -> [KeyX]
  'y' -> [KeyY]
  'z' -> [KeyZ]
  'A' -> [KeyLeftshift, KeyA]
  'B' -> [KeyLeftshift, KeyB]
  'C' -> [KeyLeftshift, KeyC]
  'D' -> [KeyLeftshift, KeyD]
  'E' -> [KeyLeftshift, KeyE]
  'F' -> [KeyLeftshift, KeyF]
  'G' -> [KeyLeftshift, KeyG]
  'H' -> [KeyLeftshift, KeyH]
  'I' -> [KeyLeftshift, KeyI]
  'J' -> [KeyLeftshift, KeyJ]
  'K' -> [KeyLeftshift, KeyK]
  'L' -> [KeyLeftshift, KeyL]
  'M' -> [KeyLeftshift, KeyM]
  'N' -> [KeyLeftshift, KeyN]
  'O' -> [KeyLeftshift, KeyO]
  'P' -> [KeyLeftshift, KeyP]
  'Q' -> [KeyLeftshift, KeyQ]
  'R' -> [KeyLeftshift, KeyR]
  'S' -> [KeyLeftshift, KeyS]
  'T' -> [KeyLeftshift, KeyT]
  'U' -> [KeyLeftshift, KeyU]
  'V' -> [KeyLeftshift, KeyV]
  'W' -> [KeyLeftshift, KeyW]
  'X' -> [KeyLeftshift, KeyX]
  'Y' -> [KeyLeftshift, KeyY]
  'Z' -> [KeyLeftshift, KeyZ]
  '0' -> [KeyKp0]
  '1' -> [KeyKp1]
  '2' -> [KeyKp2]
  '3' -> [KeyKp3]
  '4' -> [KeyKp4]
  '5' -> [KeyKp5]
  '6' -> [KeyKp6]
  '7' -> [KeyKp7]
  '8' -> [KeyKp8]
  '9' -> [KeyKp9]
  ',' -> [KeyComma]
  '.' -> [KeyDot]
  '/' -> [KeySlash]
  '-' -> [KeyMinus]
  '_' -> [KeyLeftshift, KeyMinus]
  '=' -> [KeyEqual]
  '+' -> [KeyLeftshift, KeyEqual]
  '*' -> [KeyKpasterisk]
  '[' -> [KeyLeftbrace]
  ']' -> [KeyRightbrace]
  '}' -> [KeyLeftshift, KeyRightbrace]
  '{' -> [KeyLeftshift, KeyLeftbrace]
  '(' -> [KeyLeftshift, Key9]
  ')' -> [KeyLeftshift, Key0]
  '@' -> [KeyLeftshift, Key2]
  ':' -> [KeySemicolon]
  '\'' -> [KeyApostrophe]
  '"' -> [KeyLeftshift, KeyApostrophe]
  '\\' -> [KeyBackslash]
  '\n' -> [KeyEnter]
  -- Replace unknown characters with space.
  -- Since I imagine this would be mostly used for searching, adding spaces
  -- should be safe and make the search still find what you're looking for.
  _ -> [KeySpace]

-- | Exact resolution doesn't seem to matter, think of this as the virtual
-- screen size that we use internally, but it'll get mapped onto the real one.
-- But it's important to know what the absAxes values are when setting the
-- mouse position.
screenWidth :: Int32
screenWidth = 1920

screenHeight :: Int32
screenHeight = 1080

mkInputDevice :: IO Device
mkInputDevice =
  newDevice
    "Pablo TV Input Device"
    DeviceOpts
      { phys = Nothing,
        uniq = Nothing,
        idProduct = Nothing,
        idVendor = Nothing,
        idBustype = Nothing,
        idVersion = Nothing,
        keys =
          -- We only run this once, so it's fine to use nub here
          nub
            ( [ -- Mouse buttons
                BtnLeft,
                BtnRight,
                BtnMiddle,
                -- Keyboard buttons
                KeyEsc,
                KeyBackspace,
                KeyTab,
                KeyEnter
              ]
                -- Add all keys we support in the keyboard
                ++ concatMap charToKey [minBound .. maxBound]
            ),
        relAxes = [RelX, RelY],
        absAxes =
          let absInfo max' =
                AbsInfo
                  { absValue = 0,
                    absMinimum = 0,
                    absMaximum = max',
                    absFuzz = 0,
                    absFlat = 0,
                    absResolution = 0
                  }
           in [ (AbsX, absInfo screenWidth),
                (AbsY, absInfo screenHeight)
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
