{-# LANGUAGE DerivingVia #-}

module Actions where

import Autodocodec (Autodocodec (..), Discriminator, HasCodec (..), HasObjectCodec (..), ObjectCodec, bimapCodec, discriminatedUnionCodec, eitherDecodeJSONViaCodec, encodeJSONViaCodec, mapToDecoder, mapToEncoder, object, pureCodec, requiredField', (.=))
import Autodocodec.Aeson (toJSONViaCodec)
import Control.Concurrent (tryPutMVar)
import Control.Monad (forever, when)
import DB (runDBWithConn)
import DB qualified
import Data.Char (isSpace)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.List (dropWhileEnd, nub)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as T
import Data.Time (getCurrentTime)
import Data.Void (Void)
import Database.Persist.Sqlite (PersistStoreWrite (..))
import Evdev.Codes
import Evdev.Uinput
import Foundation (App (..), Handler)
import GHC.Conc (atomically, readTVar, writeTVar)
import Path (Abs, Dir, File, Path, fromAbsDir, fromAbsFile, parseAbsDir, parseAbsFile)
import Playerctl qualified
import SafeMaths (int32ToInteger)
import System.Process (callProcess, readProcess)
import TVState (TVState (..))
import Text.Blaze qualified as Blaze
import Text.Julius (ToJavascript (..))
import Util (boundedEnumCodec)
import Yesod (FromJSON, getYesod, lift, liftIO, (=.))
import Yesod.WebSockets (WebSocketsT, receiveData)

data DirOrFile
  = Dir (Path Abs Dir)
  | File (Path Abs File)
  deriving (Show, Eq)

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
  | ActionMouseScroll Int32
  | ActionWrite String
  | ActionPlayPath DirOrFile
  | ActionMarkAsWatched DirOrFile
  | ActionMarkAsUnwatched DirOrFile
  | ActionCloseWindow
  | ActionOpenUrlOnTV Text
  | ActionRefreshTVState
  | ActionMedia Playerctl.Action
  deriving (Show, Eq)
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

      dirOrFileToStr :: DirOrFile -> String
      dirOrFileToStr = \case
        Dir dir -> fromAbsDir dir
        File file -> fromAbsFile file

      strToDirOrFile :: String -> Either String DirOrFile
      strToDirOrFile str =
        let mFile = parseAbsFile str
            mDir = parseAbsDir str
         in case (mFile, mDir) of
              (Nothing, Nothing) -> Left $ "Failed parsing dir or file: " ++ show str
              (Just file, _) -> Right $ File file
              (_, Just dir) -> Right $ Dir dir

      enc :: Action -> (Discriminator, ObjectCodec a ())
      enc = \case
        ActionClickMouse btn -> ("ClickMouse", oneFieldEncoder "button" btn)
        ActionPressKeyboard key -> ("PressKeyboard", oneFieldEncoder "key" key)
        ActionMoveMouse x y -> ("MoveMouse", twoFieldEncoder "x" x "y" y)
        ActionPointMouse lr ud -> ("PointMouse", twoFieldEncoder "leftRight" lr "upDown" ud)
        ActionMouseScroll amount -> ("MouseScroll", oneFieldEncoder "amount" amount)
        ActionWrite t -> ("Write", oneFieldEncoder "text" t)
        ActionPlayPath path -> ("PlayPath", oneFieldEncoder "path" $ dirOrFileToStr path)
        ActionMarkAsWatched path -> ("MarkAsWatched", oneFieldEncoder "path" $ dirOrFileToStr path)
        ActionMarkAsUnwatched path -> ("MarkAsUnwatched", oneFieldEncoder "path" $ dirOrFileToStr path)
        ActionCloseWindow -> ("CloseWindow", noFieldEncoder)
        ActionOpenUrlOnTV url -> ("OpenUrlOnTV", oneFieldEncoder "url" url)
        ActionRefreshTVState -> ("RefreshTVState", noFieldEncoder)
        ActionMedia url -> ("Media", oneFieldEncoder "action" url)
      dec :: HashMap.HashMap Discriminator (Text, ObjectCodec Void Action)
      dec =
        HashMap.fromList
          [ ("ClickMouse", ("ActionClickMouse", oneFieldDecoder ActionClickMouse "button")),
            ("PressKeyboard", ("ActionPressKeyboard", oneFieldDecoder ActionPressKeyboard "key")),
            ("MoveMouse", ("ActionMoveMouse", twoFieldDecoder ActionMoveMouse "x" "y")),
            ("PointMouse", ("ActionPointMouse", twoFieldDecoder ActionPointMouse "leftRight" "upDown")),
            ("MouseScroll", ("ActionMouseScroll", oneFieldDecoder ActionMouseScroll "amount")),
            ("Write", ("ActionWrite", oneFieldDecoder ActionWrite "text")),
            ( "PlayPath",
              ( "ActionPlayPath",
                mapToDecoder ActionPlayPath $
                  bimapCodec strToDirOrFile id $
                    requiredField' "path"
              )
            ),
            ( "MarkAsWatched",
              ( "ActionMarkAsWatched",
                mapToDecoder ActionMarkAsWatched $
                  bimapCodec strToDirOrFile id $
                    requiredField' "path"
              )
            ),
            ( "MarkAsUnwatched",
              ( "ActionMarkAsUnwatched",
                mapToDecoder ActionMarkAsUnwatched $
                  bimapCodec strToDirOrFile id $
                    requiredField' "path"
              )
            ),
            ("CloseWindow", ("ActionCloseWindow", noFieldDecoder ActionCloseWindow)),
            ("OpenUrlOnTV", ("ActionOpenUrlOnTV", oneFieldDecoder ActionOpenUrlOnTV "url")),
            ("RefreshTVState", ("ActionRefreshTVState", noFieldDecoder ActionRefreshTVState)),
            ("Media", ("ActionMedia", oneFieldDecoder ActionMedia "action"))
          ]

instance ToJavascript Action where
  toJavascript = toJavascript . toJSONViaCodec

instance Blaze.ToMarkup Action where
  toMarkup = Blaze.lazyText . T.decodeUtf8 . encodeJSONViaCodec

data MouseButton = MouseButtonLeft | MouseButtonRight
  deriving (Show, Eq, Bounded, Enum)
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
  | KeyboardDelete
  | KeyboardEnter
  | KeyboardLeftArrow
  | KeyboardRightArrow
  | KeyboardUpArrow
  | KeyboardDownArrow
  | KeyboardVolumeUp
  | KeyboardVolumeDown
  deriving (Show, Eq, Bounded, Enum)

instance HasCodec KeyboardButton where
  codec =
    boundedEnumCodec $ \case
      KeyboardBackspace -> "backspace"
      KeyboardDelete -> "delete"
      KeyboardEnter -> "enter"
      KeyboardLeftArrow -> "left"
      KeyboardRightArrow -> "right"
      KeyboardUpArrow -> "up"
      KeyboardDownArrow -> "down"
      KeyboardVolumeUp -> "volumeUp"
      KeyboardVolumeDown -> "volumeDown"

keyboardButtonToEvdevKey :: KeyboardButton -> Key
keyboardButtonToEvdevKey = \case
  KeyboardBackspace -> KeyBackspace
  KeyboardDelete -> KeyDelete
  KeyboardEnter -> KeyEnter
  KeyboardLeftArrow -> KeyLeft
  KeyboardRightArrow -> KeyRight
  KeyboardUpArrow -> KeyUp
  KeyboardDownArrow -> KeyDown
  KeyboardVolumeUp -> KeyVolumeup
  KeyboardVolumeDown -> KeyVolumedown

actionsWebSocket :: WebSocketsT Handler ()
actionsWebSocket = forever $ do
  d <- receiveData
  case eitherDecodeJSONViaCodec d of
    Left err -> liftIO $ putStrLn $ "Error decoding action: " <> err
    Right action -> lift $ performAction action

performAction :: Action -> Handler ()
performAction action = do
  app <- getYesod
  liftIO $ performActionIO app action

performActionIO :: App -> Action -> IO ()
performActionIO app action = do
  liftIO $ putStrLn $ "Performing action: " <> show action
  case action of
    ActionClickMouse btn' ->
      let btn = mouseButtonToEvdevKey btn'
       in liftIO $
            writeBatch
              inputDevice
              [ KeyEvent btn Pressed,
                SyncEvent SynReport,
                KeyEvent btn Released
                -- Batch automatically adds a sync at the end too
              ]
    ActionPressKeyboard key' ->
      let key = keyboardButtonToEvdevKey key'
       in liftIO $
            writeBatch
              inputDevice
              [ KeyEvent key Pressed,
                SyncEvent SynReport,
                KeyEvent key Released
                -- Batch automatically adds a sync at the end too
              ]
    ActionMoveMouse x y ->
      liftIO $
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
      liftIO $
        writeBatch
          inputDevice
          [ AbsoluteEvent AbsX $ EventValue pointerX,
            AbsoluteEvent AbsY $ EventValue pointerY
          ]
    ActionMouseScroll amount ->
      liftIO $
        writeBatch
          inputDevice
          [ RelativeEvent RelWheel $ EventValue amount
          ]
    ActionWrite text -> do
      let events = concatMap (clickKeyCombo . charToKey) text
      liftIO $ writeBatch inputDevice events
    ActionPlayPath dirOrFile -> liftIO $ do
      defaultVideoPlayer' <- readProcess "xdg-mime" ["query", "default", "video/x-msvideo"] ""
      let defaultVideoPlayer = dropWhileEnd isSpace defaultVideoPlayer'
      callProcess
        "gtk-launch"
        [ defaultVideoPlayer,
          case dirOrFile of
            File path -> fromAbsFile path
            Dir path -> fromAbsDir path
        ]
    ActionMarkAsWatched dirOrFile ->
      markDirOrFileAsWatched dirOrFile
    ActionMarkAsUnwatched dirOrFile ->
      markDirOrFileAsUnwatched dirOrFile
    ActionCloseWindow ->
      liftIO $ writeBatch inputDevice $ clickKeyCombo [KeyLeftctrl, KeyQ]
    ActionOpenUrlOnTV url ->
      liftIO $ atomically $ do
        tvState <- readTVar tvStateTVar
        writeTVar tvStateTVar $ tvState {tvPage = url}
    ActionRefreshTVState -> liftIO $ do
      success <- tryPutMVar videoDataRefreshTrigger ()
      when (not success) $
        putStrLn "Already refreshing"
    ActionMedia playerCtlAction ->
      liftIO $ Playerctl.performAction playerCtlAction
  where
    inputDevice = app.appInputDevice
    tvStateTVar = app.appTVState
    videoDataRefreshTrigger = app.appVideoDataRefreshTrigger

    clickKeyCombo keys =
      map (`KeyEvent` Pressed) keys
        ++ [SyncEvent SynReport]
        ++ map (`KeyEvent` Released) (reverse keys)
        ++ [SyncEvent SynReport]

    markDirOrFileAsWatched :: DirOrFile -> IO ()
    markDirOrFileAsWatched dirOrFile = do
      now <- liftIO getCurrentTime
      runDBWithConn app.appSqlPool $ case dirOrFile of
        Dir path -> do
          pure ()
        File path ->
          update (DB.VideoFileKey path) [DB.VideoFileWatched =. Just now]

    markDirOrFileAsUnwatched :: DirOrFile -> IO ()
    markDirOrFileAsUnwatched dirOrFile =
      runDBWithConn app.appSqlPool $ case dirOrFile of
        Dir path -> do
          pure ()
        File path ->
          update (DB.VideoFileKey path) [DB.VideoFileWatched =. Nothing]

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
          -- We can't just enable all keys as there seems to be a 255 keys
          -- limit, and there are over 500 keys.
          -- We only run this once, so it's fine to use nub here
          nub
            ( -- All keys supported by the mouse
              fmap mouseButtonToEvdevKey [minBound .. maxBound]
                -- All extra keys supported as single press keyboard keys
                ++ fmap keyboardButtonToEvdevKey [minBound .. maxBound]
                -- Add all keys we support in the keyboard
                ++ concatMap charToKey [minBound .. maxBound]
                -- Some extra control keys:
                ++ [KeyLeftctrl, KeyLeftshift]
            ),
        relAxes = [RelX, RelY, RelWheel],
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
