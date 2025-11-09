{-# LANGUAGE TemplateHaskell #-}

module Actions where

import Control.Concurrent (modifyMVar_)
import Control.Exception (Exception (..))
import Control.Monad (forever)
import Data.Aeson (ToJSON (..), eitherDecode, encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Char (isSpace)
import Data.Int (Int32)
import Data.List (dropWhileEnd, nub)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as T
import Data.Time (getCurrentTime)
import Data.Vector qualified as Vector
import Directory
  ( Directory (..),
    DirectoryPath (..),
    VideoFile (..),
    VideoFilePath (..),
    directoryPathToAbsPath,
    updateDirectoryAtPath,
    videoFilePathToAbsPath,
  )
import Evdev.Codes
import Evdev.Uinput
import Foundation (App (..), Handler)
import GHC.Conc (atomically, readTVar, writeTVar)
import Logging (LogLevel (..), putLog)
import Mpris qualified
import Network.WebSockets qualified as WS
import SafeMaths (int32ToInteger)
import System.Process (callProcess, readProcess)
import TVState (TVState (..))
import Text.Blaze qualified as Blaze
import Text.Julius (ToJavascript (..))
import UnliftIO.Exception (catch)
import Yesod (getYesod, lift, liftIO)
import Yesod.WebSockets (WebSocketsT, receiveData)

data DirOrFile
  = Dir DirectoryPath
  | File VideoFilePath
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
  | ActionOpenUrlOnTV Text
  | ActionRefreshAllDirectoryData
  | -- | ActionRefreshDirectoryData DirectoryPath
    ActionMedia Mpris.MprisAction
  deriving (Show, Eq)

data MouseButton = MouseButtonLeft | MouseButtonRight
  deriving (Show, Eq, Bounded, Enum)

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

$(deriveJSON defaultOptions ''DirOrFile)
$(deriveJSON defaultOptions ''KeyboardButton)
$(deriveJSON defaultOptions ''MouseButton)
$(deriveJSON defaultOptions ''Action)

instance ToJavascript Action where
  toJavascript = toJavascript . toJSON

instance Blaze.ToMarkup Action where
  toMarkup = Blaze.lazyText . T.decodeUtf8 . encode

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
          Left err -> liftIO $ putLog Error $ "Error decoding action: " <> err
          Right action -> lift $ performAction action

    errorHandler = \case
      WS.CloseRequest code reason -> do
        liftIO $ putLog Info $ "Received Websocket Close Request, code: " ++ show code ++ ", reason: " ++ show reason
      WS.ConnectionClosed -> do
        liftIO $ putLog Info "Websocket closed"
      err -> do
        liftIO $ putLog Error $ "Got some websocket error: " ++ displayException err

performAction :: Action -> Handler ()
performAction action = do
  app <- getYesod
  liftIO $ performActionIO app action

performActionIO :: App -> Action -> IO ()
performActionIO app action = do
  liftIO $ putLog Debug $ "Performing action: " <> show action
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
      absPath <- dirOrFileToAbsPath dirOrFile
      callProcess
        "gtk-launch"
        [ defaultVideoPlayer,
          absPath
        ]
    ActionMarkAsWatched dirOrFile ->
      markDirOrFileAsWatched dirOrFile
    ActionMarkAsUnwatched dirOrFile ->
      markDirOrFileAsUnwatched dirOrFile
    ActionOpenUrlOnTV url ->
      liftIO $ atomically $ do
        tvState <- readTVar tvStateTVar
        writeTVar tvStateTVar $ tvState {tvPage = url}
    ActionRefreshAllDirectoryData -> liftIO $ do
      -- -- To prevent spamming this, we only start a new full refresh when the queue is empty
      -- sambaShares <- runDBPool app.appSqlPool $ selectList [] []
      -- -- let getMountPath (SambaShare svr shr) = mkMountPath svr shr
      -- sambaPaths <- mapMaybeM (getMountPath . entityVal) sambaShares
      -- homeDir <- getHomeDir
      -- -- let localVideosPath = homeDir </> $(mkRelDir "Videos")
      -- let allPaths = localVideosPath : sambaPaths
      -- startedRefresh <- atomically $ do
      --   isEmpty <- isEmptyTQueue $ appDirExplorationQueue app
      --   when isEmpty . forM_ allPaths $
      --     writeTQueue (appDirExplorationQueue app) . DirToExplore
      --   pure isEmpty
      -- when (not startedRefresh) $
      --   putLog Warning "Already refreshing"
      putLog Warning "TODO: Reimplement this"
    -- ActionRefreshDirectoryData path -> liftIO $ do
    --   -- -- To prevent spamming this, we only start a new full refresh when the queue is empty
    --   -- startedRefresh <- atomically $ do
    --   --   isEmpty <- isEmptyTQueue $ appDirExplorationQueue app
    --   --   when isEmpty $
    --   --     writeTQueue (appDirExplorationQueue app) $
    --   --       DirToExplore path
    --   --   pure isEmpty
    --   -- when (not startedRefresh) $
    --   --   putLog Warning "Already refreshing"
    --   putLog Warning "TODO: Reimplement this"
    ActionMedia a ->
      liftIO $ Mpris.performAction a
  where
    inputDevice = app.appInputDevice
    tvStateTVar = app.appTVState

    clickKeyCombo keys =
      map (`KeyEvent` Pressed) keys
        ++ [SyncEvent SynReport]
        ++ map (`KeyEvent` Released) (reverse keys)
        ++ [SyncEvent SynReport]

    markDirOrFileAsWatched :: DirOrFile -> IO ()
    markDirOrFileAsWatched dirOrFile = modifyMVar_ app.appRootDirs $ \roots -> do
      let dirPath = case dirOrFile of
            Dir p -> p
            File (VideoFilePath r p _) -> DirectoryPath r p
      now <- liftIO getCurrentTime
      let applyWatched fileData =
            case fileData.videoFileWatched of
              Just _ -> fileData
              Nothing -> fileData {videoFileWatched = Just now}
      pure $ updateDirectoryAtPath roots dirPath $ \dir ->
        case dirOrFile of
          Dir _ ->
            dir
              { directoryVideoFiles =
                  Vector.map applyWatched dir.directoryVideoFiles
              }
          File (VideoFilePath _ _ fileName) ->
            dir
              { directoryVideoFiles =
                  Vector.map
                    ( \v ->
                        if v.videoFileName == fileName
                          then applyWatched v
                          else v
                    )
                    dir.directoryVideoFiles
              }

    markDirOrFileAsUnwatched :: DirOrFile -> IO ()
    markDirOrFileAsUnwatched dirOrFile = modifyMVar_ app.appRootDirs $ \roots -> do
      let dirPath = case dirOrFile of
            Dir p -> p
            File (VideoFilePath r p _) -> DirectoryPath r p
      let applyUnwatched fileData =
            case fileData.videoFileWatched of
              Just _ -> fileData {videoFileWatched = Nothing}
              Nothing -> fileData
      pure $ updateDirectoryAtPath roots dirPath $ \dir ->
        case dirOrFile of
          Dir _ ->
            dir
              { directoryVideoFiles =
                  Vector.map applyUnwatched dir.directoryVideoFiles
              }
          File (VideoFilePath _ _ fileName) ->
            dir
              { directoryVideoFiles =
                  Vector.map
                    ( \v ->
                        if v.videoFileName == fileName
                          then applyUnwatched v
                          else v
                    )
                    dir.directoryVideoFiles
              }

    dirOrFileToAbsPath :: DirOrFile -> IO FilePath
    dirOrFileToAbsPath = \case
      Dir d -> directoryPathToAbsPath d
      File f -> videoFilePathToAbsPath f

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
