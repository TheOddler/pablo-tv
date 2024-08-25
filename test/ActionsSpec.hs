module ActionsSpec where

import Actions
import Autodocodec (eitherDecodeJSONViaCodec)
import Data.ByteString.Lazy.Char8 qualified as BS
import Test.Syd

spec :: Spec
spec = do
  describe "Decoding from json" $ do
    mapM_ decodeSpec decodeExamples

decodeSpec :: (BS.ByteString, Action) -> Spec
decodeSpec (json, action) =
  it ("can decode " <> BS.unpack json) $
    eitherDecodeJSONViaCodec json `shouldBe` Right action

decodeExamples :: [(BS.ByteString, Action)]
decodeExamples =
  [ ( "{\"tag\":\"ClickMouse\"}",
      ClickMouse
    ),
    ( "{\"tag\":\"MoveMouse\",\"x\":1,\"y\":2}",
      MoveMouse 1 2
    ),
    ( "{\"y\":34,\"x\":-12,\"tag\":\"MoveMouse\"}",
      MoveMouse (-12) 34
    ),
    ( "{\"tag\":\"PointMouse\",\"leftRight\":1.2,\"upDown\":3.4}",
      PointMouse 1.2 3.4
    ),
    ( "{\"upDown\":76.5,\"leftRight\":-0.98,\"tag\":\"PointMouse\"}",
      PointMouse (-0.98) 76.5
    ),
    ( "{\"tag\":\"Write\",\"text\":\"Hello\"}",
      Write "Hello"
    ),
    ( "{\"tag\":\"Write\",\"text\":\"w () r |_ |}\"}",
      Write "w () r |_ |}"
    )
  ]
