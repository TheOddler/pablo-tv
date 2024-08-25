module ActionsSpec where

import Actions
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BS
import Test.Syd

spec :: Spec
spec = do
  describe "Decoding from json" $ do
    mapM_ decodeSpec decodeExamples

decodeSpec :: (BS.ByteString, Action) -> Spec
decodeSpec (json, action) =
  it ("can decode " <> BS.unpack json) $
    JSON.eitherDecode json `shouldBe` Right action

decodeExamples :: [(BS.ByteString, Action)]
decodeExamples =
  [ ( "{\"tag\":\"ClickMouse\"}",
      ClickMouse
    ),
    ( "{\"tag\":\"MoveMouse\",\"x\":1,\"y\":2}",
      MoveMouse 1 2
    ),
    ( "{\"tag\":\"MoveMouse\",\"x\":-12,\"y\":34}",
      MoveMouse (-12) 34
    ),
    ( "{\"tag\":\"PointMouse\",\"leftRight\":1.2,\"upDown\":3.4}",
      PointMouse 1.2 3.4
    ),
    ( "{\"tag\":\"PointMouse\",\"leftRight\":-0.98,\"upDown\":76.5}",
      PointMouse (-0.98) 76.5
    ),
    ( "{\"tag\":\"Write\",\"text\":\"Hello\"}",
      Write "Hello"
    ),
    ( "{\"tag\":\"Write\",\"text\":\"w () r |_ |}\"}",
      Write "w () r |_ |}"
    )
  ]
