{-# LANGUAGE DerivingVia #-}

module Actions where

import Autodocodec (Autodocodec (..), Discriminator, HasCodec (..), HasObjectCodec (..), ObjectCodec, discriminatedUnionCodec, eitherDecodeJSONViaCodec, mapToDecoder, mapToEncoder, object, pureCodec, requiredField', (.=))
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.List (nub)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Void (Void)
import Evdev.Codes
import Evdev.Uinput
import GHC.Generics (Generic)
import MPV (MPV, MPVCommand (..), sendCommand)
import SafeMaths (int32ToInteger)
import Util (boundedEnumCodec)
import Yesod (FromJSON, MonadHandler, liftIO)
import Yesod.WebSockets (WebSocketsT, receiveData)

data Action
  = ActionClickMouse MouseButton
  | ActionPressKeyboard KeyboardButton
  | ActionMoveMouse Int32 Int32 -- x,y
  | -- | Point the mouse relative to the center of the screen
    -- So (0,0) is the center of the screen
    -- Then, assuming the screen is wider then high, (0, 1) means middle top
    -- and (1, 0) means equally far to right right as the top is from the center,
    -- so something like (1.6, 0) would be center outer right
    -- Does that make sense?
    -- It's meant to be used with the mouse pointer tool
    ActionPointMouse Scientific Scientific -- leftRight, upDown
  | ActionWrite String
  | ActionMPV MPVCommand
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via (Autodocodec Action)

instance HasCodec Action where
  codec = object "Action" objectCodec

instance HasObjectCodec Action where
  objectCodec = discriminatedUnionCodec "tag" enc dec
    where
      nothingCodec = pureCodec ()
      twoFieldCodec first second = (,) <$> requiredField' first .= fst <*> requiredField' second .= snd

      noFieldEncoder = mapToEncoder () nothingCodec
      oneFieldEncoder name value = mapToEncoder value (requiredField' name)
      twoFieldEncoder firstName first secondName second =
        mapToEncoder (first, second) (twoFieldCodec firstName secondName)

      noFieldDecoder constructor = mapToDecoder (const constructor) nothingCodec
      oneFieldDecoder constructor name = mapToDecoder constructor (requiredField' name)
      twoFieldDecoder constructor firstName secondName =
        mapToDecoder (uncurry constructor) (twoFieldCodec firstName secondName)

      enc :: Action -> (Discriminator, ObjectCodec a ())
      enc = \case
        ActionClickMouse btn -> ("ClickMouse", oneFieldEncoder "button" btn)
        ActionPressKeyboard key -> ("PressKeyboard", oneFieldEncoder "key" key)
        ActionMoveMouse x y -> ("MoveMouse", twoFieldEncoder "x" x "y" y)
        ActionPointMouse lr ud -> ("PointMouse", twoFieldEncoder "leftRight" lr "upDown" ud)
        ActionWrite t -> ("Write", oneFieldEncoder "text" t)
        ActionMPV MPVCommandTogglePlay -> ("TogglePlay", noFieldEncoder)
        ActionMPV (MPVCommandChangeVolume change) -> ("ChangeVolume", oneFieldEncoder "change" change)
        ActionMPV (MPVCommandSeek change) -> ("Seek", oneFieldEncoder "change" change)
        ActionMPV (MPVCommandPlayPath path) -> ("PlayPath", oneFieldEncoder "path" path)
        ActionMPV MPVCommandQuit -> ("CloseMPV", noFieldEncoder)
        ActionMPV MPVCommandPlaylistNext -> ("PlaylistNext", noFieldEncoder)
        ActionMPV MPVCommandPlaylistPrevious -> ("PlaylistPrevious", noFieldEncoder)
        ActionMPV MPVCommandToggleFullscreen -> ("ToggleFullscreen", noFieldEncoder)
        ActionMPV (MPVCommandSetFullscreen fullscreen) -> ("SetFullscreen", oneFieldEncoder "fullscreen" fullscreen)
      dec :: HashMap.HashMap Discriminator (Text, ObjectCodec Void Action)
      dec =
        HashMap.fromList
          [ ("ClickMouse", ("ActionClickMouse", oneFieldDecoder ActionClickMouse "button")),
            ("PressKeyboard", ("ActionPressKeyboard", oneFieldDecoder ActionPressKeyboard "key")),
            ("MoveMouse", ("ActionMoveMouse", twoFieldDecoder ActionMoveMouse "x" "y")),
            ("PointMouse", ("ActionPointMouse", twoFieldDecoder ActionPointMouse "leftRight" "upDown")),
            ("Write", ("ActionWrite", oneFieldDecoder ActionWrite "text")),
            ("TogglePlay", ("ActionMPV TogglePlay", noFieldDecoder (ActionMPV MPVCommandTogglePlay))),
            ("ChangeVolume", ("ActionMPV ChangeVolume", oneFieldDecoder (ActionMPV . MPVCommandChangeVolume) "change")),
            ("Seek", ("ActionMPV Seek", oneFieldDecoder (ActionMPV . MPVCommandSeek) "change")),
            ("PlayPath", ("ActionMPV PlayPath", oneFieldDecoder (ActionMPV . MPVCommandPlayPath) "path")),
            ("CloseMPV", ("ActionMPV Quit", noFieldDecoder $ ActionMPV MPVCommandQuit)),
            ("PlaylistNext", ("ActionMPV PlaylistNext", noFieldDecoder $ ActionMPV MPVCommandPlaylistNext)),
            ("PlaylistPrevious", ("ActionMPV PlaylistPrevious", noFieldDecoder $ ActionMPV MPVCommandPlaylistPrevious)),
            ("ToggleFullscreen", ("ActionMPV ToggleFullscreen", noFieldDecoder $ ActionMPV MPVCommandToggleFullscreen)),
            ("SetFullscreen", ("ActionMPV SetFullscreen", oneFieldDecoder (ActionMPV . MPVCommandSetFullscreen) "fullscreen"))
          ]

data MouseButton = MouseButtonLeft | MouseButtonRight
  deriving (Show, Eq, Bounded, Enum, Generic)
  deriving (FromJSON) via (Autodocodec MouseButton)

instance HasCodec MouseButton where
  codec =
    boundedEnumCodec $ \case
      MouseButtonLeft -> "left"
      MouseButtonRight -> "right"

mouseButtonToEvdevKey :: MouseButton -> Key
mouseButtonToEvdevKey = \case
  MouseButtonLeft -> BtnLeft
  MouseButtonRight -> BtnRight

data KeyboardButton
  = KeyboardBackspace
  | KeyboardLeftArrow
  | KeyboardRightArrow
  deriving (Show, Eq, Bounded, Enum, Generic)

instance HasCodec KeyboardButton where
  codec =
    boundedEnumCodec $ \case
      KeyboardBackspace -> "backspace"
      KeyboardLeftArrow -> "left"
      KeyboardRightArrow -> "right"

keyboardButtonToEvdevKey :: KeyboardButton -> Key
keyboardButtonToEvdevKey = \case
  KeyboardBackspace -> KeyBackspace
  KeyboardLeftArrow -> KeyLeft
  KeyboardRightArrow -> KeyRight

actionsWebSocket :: (MonadHandler m) => Device -> MPV -> WebSocketsT m ()
actionsWebSocket inputDevice mpv = forever $ do
  d <- receiveData
  liftIO $ decodeAndPerformAction inputDevice mpv d

decodeAndPerformAction :: Device -> MPV -> ByteString -> IO ()
decodeAndPerformAction inputDevice mpv websocketData = case eitherDecodeJSONViaCodec websocketData of
  Left err -> putStrLn $ "Error decoding action: " <> err
  Right action -> performAction inputDevice mpv action

performAction :: Device -> MPV -> Action -> IO ()
performAction inputDevice mpv = \case
  ActionClickMouse btn' ->
    let btn = mouseButtonToEvdevKey btn'
     in writeBatch
          inputDevice
          [ KeyEvent btn Pressed,
            SyncEvent SynReport,
            KeyEvent btn Released
            -- Batch automatically adds a sync at the end too
          ]
  ActionPressKeyboard key' ->
    let key = keyboardButtonToEvdevKey key'
     in writeBatch
          inputDevice
          [ KeyEvent key Pressed,
            SyncEvent SynReport,
            KeyEvent key Released
          ]
  ActionMoveMouse x y ->
    writeBatch
      inputDevice
      [ RelativeEvent RelX $ EventValue x,
        RelativeEvent RelY $ EventValue y
      ]
  ActionPointMouse lr ud -> do
    let -- I assume the screen is in landscape mode
        fromInt32 int32 = scientific (int32ToInteger int32) 0
        screenHalfSize = fromInt32 screenHeight / 2
        centerX = fromInt32 screenWidth / 2
        centerY = fromInt32 screenHeight / 2
        pointerX = floor $ centerX + lr * screenHalfSize
        pointerY = floor $ centerY + ud * screenHalfSize
    writeBatch
      inputDevice
      [ AbsoluteEvent AbsX $ EventValue pointerX,
        AbsoluteEvent AbsY $ EventValue pointerY
      ]
  ActionWrite text -> do
    let clickKeyCombo keys =
          map (`KeyEvent` Pressed) keys
            ++ [SyncEvent SynReport]
            ++ map (`KeyEvent` Released) (reverse keys)
            ++ [SyncEvent SynReport]
        events = concatMap (clickKeyCombo . charToKey) text
    writeBatch inputDevice events
  ActionMPV command -> sendCommand mpv command

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
            ( -- All keys supported by the mouse
              fmap mouseButtonToEvdevKey [minBound .. maxBound]
                -- All extra keys supported as single press keyboard keys
                ++ fmap keyboardButtonToEvdevKey [minBound .. maxBound]
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
