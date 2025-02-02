{-# OPTIONS_GHC -Wno-orphans #-}

module ActionsSpec where

import Actions
import Autodocodec (eitherDecodeJSONViaCodec, encodeJSONViaCodec)
import Data.ByteString.Lazy.Char8 qualified as BS
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Path (Abs, Dir, File, Path)
import Test.QuickCheck (Arbitrary (..), property)
import Test.QuickCheck.Instances ()
import Test.Syd
import TestUtils (forceAbsDir)

deriving instance Generic Action

deriving instance Generic DirOrFile

deriving instance Generic MouseButton

deriving instance Generic KeyboardButton

instance Arbitrary (Path Abs File) where
  arbitrary = genericArbitrary uniform

instance Arbitrary (Path Abs Dir) where
  arbitrary = genericArbitrary uniform

instance Arbitrary DirOrFile where
  arbitrary = genericArbitrary uniform

instance Arbitrary Action where
  arbitrary = genericArbitrary uniform

instance Arbitrary MouseButton where
  arbitrary = genericArbitrary uniform

instance Arbitrary KeyboardButton where
  arbitrary = genericArbitrary uniform

spec :: Spec
spec = do
  describe "Decoding from json" $ do
    mapM_ decodeSpec decodeExamples

  it "can roundtrip JSON" $ property $ \(action :: Action) ->
    let encoded = encodeJSONViaCodec action
     in eitherDecodeJSONViaCodec encoded `shouldBe` Right action

decodeSpec :: (BS.ByteString, Action) -> Spec
decodeSpec (json, action) =
  it ("can decode " <> BS.unpack json) $
    eitherDecodeJSONViaCodec json `shouldBe` Right action

decodeExamples :: [(BS.ByteString, Action)]
decodeExamples =
  [ ( "{\"tag\":\"ClickMouse\", \"button\": \"left\"}",
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
    ( "{\"tag\":\"PlayPath\",\"path\":\"/path/to/file/or/folder/\"}",
      ActionPlayPath $ Dir $ forceAbsDir "/path/to/file/or/folder/"
    )
  ]
