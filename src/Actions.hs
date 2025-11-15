{-# LANGUAGE OverloadedStrings #-}

module Actions where

import Control.Applicative ((<|>))
import Control.Exception (Exception (..))
import Control.Monad (forever)
import Data.Aeson (Encoding, FromJSON (..), Object, ToJSON (..), eitherDecode, encode, genericParseJSON, genericToEncoding, pairs, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isSpace)
import Data.Int (Int32)
import Data.List (dropWhileEnd, nub)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Time (getCurrentTime)
import Directory
  ( DirectoryUpdateResult (..),
    RootDirectories,
    getDirectoryAtPath,
    saveRootsToDisk,
    updateDirectoryAtPath,
    updateDirectoryFromDisk,
    updateRootDirectoriesFromDisk,
  )
import Directory.Directories (DirectoryData (..))
import Directory.Files (VideoFileData (..))
import Directory.Paths (DirectoryPath (..), VideoFilePath (..), directoryPathToAbsPath, videoFilePathToAbsPath)
import Evdev.Codes
import Evdev.Uinput
import Foundation (App (..), Handler)
import GHC.Conc (atomically, readTVar, writeTVar)
import GHC.Generics (Generic)
import Logging (LogLevel (..), putLog)
import Mpris qualified
import Network.WebSockets qualified as WS
import PVar (modifyPVar_, tryModifyPVar)
import SafeConvert (int32ToInteger)
import System.Process (callProcess, readProcess)
import TVState (TVState (..))
import Text.Blaze qualified as Blaze
import Text.Julius (ToJavascript (..))
import UnliftIO.Exception (catch)
import Util (fail404, impossible, logDuration, ourAesonOptionsPrefix)
import Yesod (MonadIO, getYesod, lift, liftIO)
import Yesod.WebSockets (WebSocketsT, receiveData)

data DirOrFile
  = Dir DirectoryPath
  | File VideoFilePath
  deriving (Show, Eq, Generic)

instance ToJSON DirOrFile where
  toEncoding = \case
    Dir d -> toEncoding d
    File f -> toEncoding f

instance FromJSON DirOrFile where
  parseJSON v = File <$> parseJSON v <|> Dir <$> parseJSON v

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
  | ActionWrite {text :: String}
  | ActionPlayPath DirOrFile
  | ActionMarkAsWatched DirOrFile
  | ActionMarkAsUnwatched DirOrFile
  | ActionOpenUrlOnTV Text
  | ActionRefreshAllDirectoryData
  | ActionRefreshDirectoryData DirectoryPath
  | ActionMedia Mpris.MprisAction
  deriving (Show, Eq, Generic)

instance ToJSON Action where
  toEncoding = \case
    ActionClickMouse button -> oneField "ClickMouse" "button" button
    ActionPressKeyboard button -> oneField "PressKeyboard" "button" button
    ActionMoveMouse x y -> twoFields "MoveMouse" "x" x "y" y
    ActionPointMouse leftRight upDown -> twoFields "PointMouse" "leftRight" leftRight "upDown" upDown
    ActionMouseScroll amount -> oneField "MouseScroll" "amount" amount
    ActionWrite text -> oneField "Write" "text" text
    ActionPlayPath path -> oneField "PlayPath" "path" path
    ActionMarkAsWatched path -> oneField "MarkAsWatched" "path" path
    ActionMarkAsUnwatched path -> oneField "MarkAsUnwatched" "path" path
    ActionOpenUrlOnTV url -> oneField "OpenUrlOnTV" "url" url
    ActionRefreshAllDirectoryData -> noFields "RefreshAllDirectoryData"
    ActionRefreshDirectoryData path -> oneField "RefreshDirectoryData" "path" path
    ActionMedia mpris -> oneField "Media" "action" mpris
    where
      noFields :: Text -> Encoding
      noFields tag = pairs ("tag" .= tag)
      oneField :: (ToJSON a) => Text -> Aeson.Key -> a -> Encoding
      oneField tag field value = pairs ("tag" .= tag <> field .= value)
      twoFields :: (ToJSON a, ToJSON b) => Text -> Aeson.Key -> a -> Aeson.Key -> b -> Encoding
      twoFields tag f1 v1 f2 v2 = pairs ("tag" .= tag <> f1 .= v1 <> f2 .= v2)

instance FromJSON Action where
  parseJSON = withObject "Action" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "ClickMouse" -> oneField obj ActionClickMouse "button"
      "PressKeyboard" -> oneField obj ActionPressKeyboard "button"
      "MoveMouse" -> twoFields obj ActionMoveMouse "x" "y"
      "PointMouse" -> twoFields obj ActionPointMouse "leftRight" "upDown"
      "MouseScroll" -> oneField obj ActionMouseScroll "amount"
      "Write" -> oneField obj ActionWrite "text"
      "PlayPath" -> oneField obj ActionPlayPath "path"
      "MarkAsWatched" -> oneField obj ActionMarkAsWatched "path"
      "MarkAsUnwatched" -> oneField obj ActionMarkAsUnwatched "path"
      "OpenUrlOnTV" -> oneField obj ActionOpenUrlOnTV "url"
      "RefreshAllDirectoryData" -> noFields ActionRefreshAllDirectoryData
      "RefreshDirectoryData" -> oneField obj ActionRefreshDirectoryData "path"
      "Media" -> oneField obj ActionMedia "action"
      unknownTag -> fail $ "Unknown Action tag: " ++ T.unpack unknownTag
    where
      noFields :: Action -> Parser Action
      noFields = pure
      oneField :: (FromJSON a) => Object -> (a -> Action) -> Aeson.Key -> Parser Action
      oneField obj constructor field = do
        value <- obj .: field
        pure $ constructor value
      twoFields :: (FromJSON a, FromJSON b) => Object -> (a -> b -> Action) -> Aeson.Key -> Aeson.Key -> Parser Action
      twoFields obj constructor f1 f2 = do
        v1 <- obj .: f1
        v2 <- obj .: f2
        pure $ constructor v1 v2

instance ToJavascript Action where
  toJavascript = toJavascript . toJSON

instance Blaze.ToMarkup Action where
  toMarkup = Blaze.lazyText . T.decodeUtf8 . encode

data MouseButton = MouseButtonLeft | MouseButtonRight
  deriving (Show, Eq, Bounded, Enum, Generic)

instance ToJSON MouseButton where
  toEncoding = genericToEncoding $ ourAesonOptionsPrefix "MouseButton"

instance FromJSON MouseButton where
  parseJSON = genericParseJSON $ ourAesonOptionsPrefix "MouseButton"

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
  deriving (Show, Eq, Bounded, Enum, Generic)

instance ToJSON KeyboardButton where
  toEncoding = genericToEncoding $ ourAesonOptionsPrefix "Keyboard"

instance FromJSON KeyboardButton where
  parseJSON = genericParseJSON $ ourAesonOptionsPrefix "Keyboard"

mouseButtonToEvdevKey :: MouseButton -> Key
mouseButtonToEvdevKey = \case
  MouseButtonLeft -> BtnLeft
  MouseButtonRight -> BtnRight

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
actionsWebSocket =
  loop `catch` errorHandler
  where
    loop =
      forever $ do
        d <- receiveData
        case eitherDecode d of
          Left err -> putLog Error $ "Error decoding action: " <> err
          Right action -> lift $ performAction action

    errorHandler = \case
      WS.CloseRequest code reason -> do
        putLog Debug $ "Received Websocket Close Request, code: " ++ show code ++ ", reason: " ++ show reason
      WS.ConnectionClosed -> do
        putLog Debug "Websocket closed"
      err -> do
        putLog Error $ "Got some websocket error: " ++ displayException err

performAction :: Action -> Handler ()
performAction action = do
  app <- getYesod
  putLog Debug $ "Performing action: " <> show action
  case action of
    ActionClickMouse btn' ->
      let btn = mouseButtonToEvdevKey btn'
       in liftIO $
            writeBatch
              app.appInputDevice
              [ KeyEvent btn Pressed,
                SyncEvent SynReport,
                KeyEvent btn Released
                -- Batch automatically adds a sync at the end too
              ]
    ActionPressKeyboard key' ->
      let key = keyboardButtonToEvdevKey key'
       in liftIO $
            writeBatch
              app.appInputDevice
              [ KeyEvent key Pressed,
                SyncEvent SynReport,
                KeyEvent key Released
                -- Batch automatically adds a sync at the end too
              ]
    ActionMoveMouse x y ->
      liftIO $
        writeBatch
          app.appInputDevice
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
          app.appInputDevice
          [ AbsoluteEvent AbsX $ EventValue pointerX,
            AbsoluteEvent AbsY $ EventValue pointerY
          ]
    ActionMouseScroll amount ->
      liftIO $
        writeBatch
          app.appInputDevice
          [ RelativeEvent RelWheel $ EventValue amount
          ]
    ActionWrite text -> do
      let events = concatMap (clickKeyCombo . charToKey) text
      liftIO $ writeBatch app.appInputDevice events
    ActionPlayPath dirOrFile -> liftIO $ do
      defaultVideoPlayer' <- readProcess "xdg-mime" ["query", "default", "video/x-msvideo"] ""
      let defaultVideoPlayer = dropWhileEnd isSpace defaultVideoPlayer'
      absPath <- dirOrFileToAbsPath dirOrFile
      callProcess
        "gtk-launch"
        [ defaultVideoPlayer,
          absPath
        ]
    ActionMarkAsWatched dirOrFile ->
      modifyPVar_ app.appRootDirs $ markDirOrFileAsWatched dirOrFile
    ActionMarkAsUnwatched dirOrFile ->
      modifyPVar_ app.appRootDirs $ markDirOrFileAsUnwatched dirOrFile
    ActionOpenUrlOnTV url ->
      liftIO $ atomically $ do
        tvState <- readTVar app.appTVState
        writeTVar app.appTVState $ tvState {tvPage = url}
    ActionRefreshDirectoryData dirPath -> do
      -- To prevent spamming this, we only try to update
      -- This action can be slow, but that's fine, Yesod calls are run in their own thread anyway, and that way we have a nice way of letter the frontend know when the refresh is done.
      absDirPath <- directoryPathToAbsPath dirPath
      let logMessage = "Updated directory " ++ absDirPath ++ " data"
      mUpdatedRoots <- tryModifyPVar app.appRootDirs $ \roots -> logDuration logMessage $ do
        let mCurrentKnownData = getDirectoryAtPath roots dirPath
        case mCurrentKnownData of
          Nothing -> do
            fail404 "Trying to refresh a directory we don't actually know about. This should only be used on already known dirs for updating them."
          Just currentKnownData -> do
            updatedData <- updateDirectoryFromDisk roots dirPath currentKnownData
            case updatedData of
              DirectoryChanged d -> pure $ updateDirectoryAtPath roots dirPath (const d)
              DirectoryUnchanged -> pure roots
              DirectoryNotFoundRoot -> impossible "We already checked this dir should exist."
              DirectoryNotADirectory -> impossible "We already checked this dir should exist."

      case mUpdatedRoots of
        Nothing -> putLog Warning "Already refreshing"
        Just updatedRoots -> saveRootsToDisk updatedRoots
    ActionRefreshAllDirectoryData -> do
      -- To prevent spamming this, we only try to update
      -- This action can be slow, but that's fine, Yesod calls are run in their own thread anyway, and that way we have a nice way of letter the frontend know when the refresh is done.
      mUpdatedRoots <- tryModifyPVar app.appRootDirs $ \roots ->
        logDuration "Updated all data" $ updateRootDirectoriesFromDisk roots
      case mUpdatedRoots of
        Nothing -> putLog Warning "Already refreshing"
        Just updatedRoots -> saveRootsToDisk updatedRoots
    ActionMedia a ->
      Mpris.performAction a
  where
    clickKeyCombo keys =
      map (`KeyEvent` Pressed) keys
        ++ [SyncEvent SynReport]
        ++ map (`KeyEvent` Released) (reverse keys)
        ++ [SyncEvent SynReport]

    dirOrFileToAbsPath :: DirOrFile -> IO FilePath
    dirOrFileToAbsPath = \case
      Dir d -> directoryPathToAbsPath d
      File f -> videoFilePathToAbsPath f

markDirOrFileAsWatched :: (MonadIO m) => DirOrFile -> RootDirectories -> m RootDirectories
markDirOrFileAsWatched dirOrFile rootDirs = do
  let dirPath = case dirOrFile of
        Dir p -> p
        File (VideoFilePath r p _) -> DirectoryPath r p
  now <- liftIO getCurrentTime
  let applyWatched fileData =
        case fileData.videoFileWatched of
          Just _ -> fileData
          Nothing -> fileData {videoFileWatched = Just now}
  pure $ updateDirectoryAtPath rootDirs dirPath $ \dir ->
    case dirOrFile of
      Dir _ ->
        dir
          { directoryVideoFiles =
              Map.map applyWatched dir.directoryVideoFiles
          }
      File (VideoFilePath _ _ fileName) ->
        dir
          { directoryVideoFiles =
              Map.adjust applyWatched fileName dir.directoryVideoFiles
          }

markDirOrFileAsUnwatched :: (MonadIO m) => DirOrFile -> RootDirectories -> m RootDirectories
markDirOrFileAsUnwatched dirOrFile rootDirs = do
  let dirPath = case dirOrFile of
        Dir p -> p
        File (VideoFilePath r p _) -> DirectoryPath r p
  let applyUnwatched fileData =
        case fileData.videoFileWatched of
          Just _ -> fileData {videoFileWatched = Nothing}
          Nothing -> fileData
  pure $ updateDirectoryAtPath rootDirs dirPath $ \dir ->
    case dirOrFile of
      Dir _ ->
        dir
          { directoryVideoFiles =
              Map.map applyUnwatched dir.directoryVideoFiles
          }
      File (VideoFilePath _ _ fileName) ->
        dir
          { directoryVideoFiles =
              Map.adjust applyUnwatched fileName dir.directoryVideoFiles
          }

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

mkInputDevice :: (MonadIO m) => m Device
mkInputDevice =
  liftIO $
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
