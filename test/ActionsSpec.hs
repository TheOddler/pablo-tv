{-# LANGUAGE QuasiQuotes #-}

module ActionsSpec where

import Actions
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Directory.Directories (DirectoryName (..), RootDirectoryLocation (..))
import Directory.Files (VideoFileName (..))
import Directory.Paths (DirectoryPath (..), VideoFilePath (..))
import Orphanage ()
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (property)
import Test.QuickCheck.Instances ()
import Test.Syd
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
  describe "Decoding from json" $ do
    mapM_ decodeSpec encodingExamples

  describe "Encoding to json" $ do
    mapM_ encodeSpec encodingExamples

  it "can roundtrip JSON" $ property $ \(action :: Action) ->
    let encoded = encode action
     in eitherDecode encoded `shouldBe` Right action

decodeSpec :: (BS.ByteString, Action) -> Spec
decodeSpec (json, action) =
  it ("can decode " <> BS.unpack json) $
    eitherDecode json `shouldBe` Right action

encodeSpec :: (BS.ByteString, Action) -> Spec
encodeSpec (json, action) =
  it ("can encode " <> BS.unpack json) $ do
    -- The ordering of fields can differ, so we decode/encode through Value to normalise the examples
    (jsonValue :: Value) <- case eitherDecode json of
      Left err -> expectationFailure err
      Right v -> pure v
    (actionValue :: Value) <- case eitherDecode (encode action) of
      Left err -> expectationFailure err
      Right v -> pure v
    -- And another encode for nicer error messages
    encode actionValue `shouldBe` encode jsonValue

encodingExamples :: [(BS.ByteString, Action)]
encodingExamples =
  [ ( "{\"tag\":\"ClickMouse\", \"button\": \"Left\"}",
      ActionClickMouse MouseButtonLeft
    ),
    ( "{\"tag\":\"MoveMouse\",\"x\":1,\"y\":2}",
      ActionMoveMouse 1 2
    ),
    ( "{\"y\":34,\"x\":-12,\"tag\":\"MoveMouse\"}",
      ActionMoveMouse (-12) 34
    ),
    ( "{\"tag\":\"PointMouse\",\"leftRight\":1.2,\"upDown\":3.4}",
      ActionPointMouse 1.2 3.4
    ),
    ( "{\"upDown\":76.5,\"leftRight\":-0.98,\"tag\":\"PointMouse\"}",
      ActionPointMouse (-0.98) 76.5
    ),
    ( "{\"tag\":\"Write\",\"text\":\"Hello\"}",
      ActionWrite "Hello"
    ),
    ( "{\"tag\":\"Write\",\"text\":\"w () r |_ |}\"}",
      ActionWrite "w () r |_ |}"
    ),
    ( "{\"tag\":\"PlayPath\",\"path\":\"Videos/path/to/folder\"}",
      ActionPlayPath . Dir $
        DirectoryPath
          RootLocalVideos
          [ DirectoryName [twsQQ|path|],
            DirectoryName [twsQQ|to|],
            DirectoryName [twsQQ|folder|]
          ]
    ),
    ( "{\"tag\":\"PlayPath\",\"path\":\"smb-192.168.0.0-movies/path/to/file.mov\"}",
      ActionPlayPath . File $
        VideoFilePath
          (RootSamba (SmbServer "192.168.0.0") (SmbShare "movies"))
          [ DirectoryName [twsQQ|path|],
            DirectoryName [twsQQ|to|]
          ]
          (VideoFileName [twsQQ|file.mov|])
    )
  ]
