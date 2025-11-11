{-# LANGUAGE TemplateHaskell #-}

module Mpris where

import Control.Monad.Trans.Except (Except, ExceptT (..), runExcept)
import DBus
  ( BusName,
    InterfaceName,
    IsVariant (..),
    MemberName,
    MethodCall (..),
    MethodReturn (..),
    ObjectPath,
    Signal (..),
    Variant,
    formatBusName,
    methodCall,
  )
import DBus.Client (Client, MatchRule (..), SignalHandler, addMatch, call, connectSession, matchAny)
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Directory (VideoFilePath)
import Logging (LogLevel (..), Logger (..))
import Network.URI (unEscapeString)
import Util (ourAesonOptions)

data MprisAction
  = MprisQuit
  | MprisPlayPause
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

baseInterface :: InterfaceName
baseInterface = "org.mpris.MediaPlayer2"

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

-- | This instantly returns, and shouldn't be run in a race.
-- This is essentially a wrapper around `addMatch` so can be stopped by using the returned SignalHandler.
mediaListener :: (VideoFilePath -> IO ()) -> IO SignalHandler
mediaListener onFilePlayed = do
  client <- connectSession

  -- Add a match rule for PropertiesChanged signals on the Player interface
  let match =
        matchAny
          { matchPath = Just mprisObject,
            matchInterface = Just propertiesInterface,
            matchMember = Just "PropertiesChanged"
          }

  -- Register signal handler
  addMatch client match $ \signal -> do
    let justOrErr :: String -> Maybe a -> Except String a
        justOrErr err =
          ExceptT . \case
            Just a -> pure $ Right a
            Nothing -> pure $ Left err
        guard :: Bool -> String -> Except String ()
        guard b err =
          ExceptT $
            if b
              then pure $ Right ()
              else pure $ Left err
    let body = signalBody signal
    case body of
      [interface', changedProps', _] ->
        let errOrPath :: Either String VideoFilePath
            errOrPath = runExcept $ do
              interface <- justOrErr "interface failed parsing" $ fromVariant interface'
              guard (interface == playerInterface) $ "interface is not " ++ show playerInterface
              (changedProps :: Map.Map String Variant) <- justOrErr "changed props failed parsing" $ fromVariant changedProps'
              (metaData :: Map.Map String Variant) <-
                justOrErr "Metadata field not found in changed properties" $
                  Map.lookup ("Metadata" :: String) changedProps >>= fromVariant
              (videoLength :: Int64) <- justOrErr "length not found" $ Map.lookup "mpris:length" metaData >>= fromVariant
              -- I observed that opening a video sends 3 signals, each time with a bit more information. The last one is the only one with a proper length, so only listen for that one
              guard (videoLength > 0) "length reported as 0, so likely some in-between signal"
              (xesamUrl :: String) <- justOrErr "video path" $ Map.lookup "xesam:url" metaData >>= fromVariant
              let mFile :: Maybe VideoFilePath
                  mFile = case unEscapeString xesamUrl of
                    'f' : 'i' : 'l' : 'e' : ':' : '/' : '/' : path ->
                      -- TODO: Convert a filePath to VideoFilePath by checking what root it belongs to and stuff
                      undefined path
                    _ -> Nothing
              justOrErr "file path failed parsing" mFile
         in case errOrPath of
              Left err -> putLog Debug $ "Ignoring property changes because " ++ err ++ "; " ++ show body
              Right path -> onFilePlayed path
      _ ->
        putLog Error $ "Incorrect body shape for PropertiesChanged signal: " ++ show body

performAction :: MprisAction -> IO ()
performAction action = do
  client <- connectSession
  -- Request a list of connected clients from the bus
  mPlayer <- firstMediaPlayer client
  case mPlayer of
    Nothing -> putLog Warning "No media player found."
    Just mp -> do
      let baseCallNoParam method =
            mprisMethodCall mp baseInterface method []
          playerCallNoParam method =
            mprisMethodCall mp playerInterface method []
          playerCall method param =
            mprisMethodCall mp playerInterface method [toVariant param]
          baseSetProp :: (IsVariant a) => String -> a -> MethodCall
          baseSetProp prop val =
            mprisMethodCall
              mp
              propertiesInterface
              "Set"
              [ -- First parameter is the interface name
                toVariant baseInterface,
                -- Second the property we want to set
                toVariant prop,
                -- Third the value (as a `Variant`, so we need `toVariant` twice, as the first is unwrapped by `call`)
                toVariant $ toVariant val
              ]
          sToMicroS :: Int64 -> Int64
          sToMicroS s = s * 1000000

          methodCall' = case action of
            -- Relevant docs:
            -- https://specifications.freedesktop.org/mpris-spec/latest/index.html
            -- https://specifications.freedesktop.org/mpris-spec/latest/Media_Player.html
            -- https://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html
            MprisQuit -> baseCallNoParam "Quit"
            MprisPlayPause -> playerCallNoParam "PlayPause"
            MprisStop -> playerCallNoParam "Stop"
            MprisNext -> playerCallNoParam "Next"
            MprisPrevious -> playerCallNoParam "Previous"
            MprisForwardStep -> playerCall "Seek" $ sToMicroS 10
            MprisBackwardStep -> playerCall "Seek" $ -sToMicroS 10
            MprisForwardJump -> playerCall "Seek" $ sToMicroS 60
            MprisBackwardJump -> playerCall "Seek" $ -sToMicroS 60
            MprisGoFullscreen -> baseSetProp "Fullscreen" True
            MprisGoWindowed -> baseSetProp "Fullscreen" False

      errOrResult <- call client methodCall'
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

fromVariant2 :: (IsVariant a) => Variant -> Maybe a
fromVariant2 v = fromVariant v >>= fromVariant

$(deriveJSON ourAesonOptions ''Mpris.MprisAction)
