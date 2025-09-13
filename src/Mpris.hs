module Mpris where

import Autodocodec (HasCodec (..))
import DBus
  ( BusName,
    InterfaceName,
    IsVariant (..),
    MemberName,
    MethodCall (..),
    MethodError,
    MethodReturn (..),
    ObjectPath,
    Variant,
    formatBusName,
    methodCall,
  )
import DBus.Client (Client, call, connectSession)
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Logging (LogLevel (..), Logger (..))
import Util (boundedEnumCodec)

data MprisAction
  = MprisPlayPause
  | MprisStop
  | MprisNext
  | MprisPrevious
  | MprisForwardStep
  | MprisBackwardStep
  | MprisForwardJump
  | MprisBackwardJump
  | MprisGoFullscreen
  | MprisGoWindowed
  deriving (Show, Eq, Bounded, Enum)

instance HasCodec MprisAction where
  codec =
    boundedEnumCodec $ \case
      MprisPlayPause -> "playPause"
      MprisStop -> "stop"
      MprisNext -> "next"
      MprisPrevious -> "previous"
      MprisForwardStep -> "forwardStep"
      MprisBackwardStep -> "backwardStep"
      MprisForwardJump -> "forwardJump"
      MprisBackwardJump -> "backwardJump"
      MprisGoFullscreen -> "fullscreen"
      MprisGoWindowed -> "windowed"

newtype MediaPlayer = MediaPlayer {unMediaPlayer :: BusName}

instance Show MediaPlayer where
  show = formatBusName . unMediaPlayer

allMediaPlayers :: Client -> IO [MediaPlayer]
allMediaPlayers client = do
  reply <-
    call
      client
      (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
        { methodCallDestination = Just "org.freedesktop.DBus"
        }
  case reply of
    Left err -> do
      putLog Error $ "Error when trying to get all DBus names: " ++ show err
      pure []
    Right r -> do
      names <- expectSingleValue $ methodReturnBody r
      let mediaPlayers = filter ("org.mpris.MediaPlayer2" `isPrefixOf`) names
      pure $ MediaPlayer . fromString <$> mediaPlayers

firstMediaPlayer :: Client -> IO (Maybe MediaPlayer)
firstMediaPlayer client = listToMaybe <$> allMediaPlayers client

mprisObject :: ObjectPath
mprisObject = "/org/mpris/MediaPlayer2"

mprisInterface :: InterfaceName
mprisInterface = "org.mpris.MediaPlayer2"

playerInterface :: InterfaceName
playerInterface = "org.mpris.MediaPlayer2.Player"

propertiesInterface :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties"

mprisMethodCall :: MediaPlayer -> InterfaceName -> MemberName -> [Variant] -> MethodCall
mprisMethodCall destination interface member body =
  (methodCall mprisObject interface member)
    { methodCallDestination = Just $ unMediaPlayer destination,
      methodCallBody = body
    }

performAction :: MprisAction -> IO ()
performAction action = do
  client <- connectSession
  -- Request a list of connected clients from the bus
  mPlayer <- firstMediaPlayer client
  case mPlayer of
    Nothing -> putLog Warning "No media player found."
    Just mp -> do
      let playerCallNoParam method =
            call client $ mprisMethodCall mp playerInterface method []
          playerCall method param =
            call client $ mprisMethodCall mp playerInterface method [toVariant param]
          baseSetProp :: (IsVariant a) => String -> a -> IO (Either MethodError MethodReturn)
          baseSetProp prop val =
            call client $
              mprisMethodCall
                mp
                propertiesInterface
                "Set"
                [ -- First parameter is the interface name
                  toVariant mprisInterface,
                  -- Second the property we want to set
                  toVariant prop,
                  -- Third the value (as a `Variant`, so we need `toVariant` twice, as the first is unwrapped by `call`)
                  toVariant $ toVariant val
                ]
          sToMs :: Int32 -> Int32
          sToMs s = s * 1000

          doAction :: IO (Either MethodError MethodReturn)
          doAction = case action of
            MprisPlayPause -> playerCallNoParam "PlayPause"
            MprisStop -> playerCallNoParam "Stop"
            MprisNext -> playerCallNoParam "Next"
            MprisPrevious -> playerCallNoParam "Previous"
            MprisForwardStep -> playerCall "Seek" $ sToMs 10
            MprisBackwardStep -> playerCall "Seek" $ -sToMs 10
            MprisForwardJump -> playerCall "Seek" $ sToMs 60
            MprisBackwardJump -> playerCall "Seek" $ -sToMs 60
            MprisGoFullscreen -> baseSetProp "Fullscreen" True
            MprisGoWindowed -> baseSetProp "Fullscreen" False

      errOrResult <- doAction
      case errOrResult of
        Right result ->
          putLog Info $
            unwords
              [ "Successfully called",
                show action,
                "on",
                show mp,
                ":",
                show $ methodReturnBody result
              ]
        Left err ->
          putLog Error $
            unwords
              [ "Error when calling",
                show action,
                "on",
                show mp,
                ":",
                show err
              ]

-- | Some helpers to parse dbus replies
expectSingleValue :: (MonadFail m, IsVariant a) => [Variant] -> m a
expectSingleValue = \case
  [] -> fail "Expected single value, but got none"
  [val] -> case fromVariant val of
    Nothing -> fail "Failed parsing single variant"
    Just a -> pure a
  _ -> fail "Expected single value, but got multiple"
