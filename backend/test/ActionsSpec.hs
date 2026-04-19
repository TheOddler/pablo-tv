{-# LANGUAGE QuasiQuotes #-}

module ActionsSpec where

import Actions
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Directory.Paths (RawWebPath (..))
import Orphanage ()
import Test.QuickCheck (property, withMaxSuccess)
import Test.Syd
import Test.Syd.Aeson (pureGoldenJSONValueFile)
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
  describe "Decoding from json" $ do
    mapM_ decodeSpec encodingExamples

  describe "Encoding to json" $ do
    mapM_ encodeSpec encodingExamples

  it "can roundtrip JSON" $ withMaxSuccess 1000 $ property $ \(action :: Action) -> do
    let encoded = encode action
    context ("encoded was: " ++ show encoded) $
      eitherDecode encoded `shouldBe` Right action

  it "keeps consistent encoding" $ do
    pureGoldenJSONValueFile "test/golden/actions.json" $
      concat
        [ ActionClickMouse <$> [minBound .. maxBound],
          ActionPressKeyboard <$> [minBound .. maxBound],
          [ActionMoveMouse 4 2],
          [ActionPointMouse 1 2],
          [ActionMouseScroll 3],
          [ActionWrite "test string"],
          [ ActionPlayPath $
              RawWebPath
                [ [twsQQ|Videos|],
                  [twsQQ|path|],
                  [twsQQ|to|],
                  [twsQQ|folder|]
                ]
          ],
          [ ActionMarkAsWatched $
              RawWebPath
                [ [twsQQ|smb-192.168.0.0-movies|],
                  [twsQQ|path|],
                  [twsQQ|to|],
                  [twsQQ|file.mov|]
                ]
          ],
          [ ActionMarkAsUnwatched $
              RawWebPath
                [ [twsQQ|smb-192.168.0.0-movies|],
                  [twsQQ|path|],
                  [twsQQ|to|],
                  [twsQQ|another|],
                  [twsQQ|file.mov|]
                ]
          ],
          [ActionOpenUrlOnTV "https://www.pabloproductions.be/"],
          [ActionRefreshAllDirectoryData],
          [ ActionRefreshDirectoryData $
              RawWebPath
                [ [twsQQ|Videos|],
                  [twsQQ|path|],
                  [twsQQ|to|],
                  [twsQQ|folder 2|]
                ]
          ],
          ActionMedia <$> [minBound .. maxBound]
        ]

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
      ActionPlayPath $
        RawWebPath
          [ [twsQQ|Videos|],
            [twsQQ|path|],
            [twsQQ|to|],
            [twsQQ|folder|]
          ]
    ),
    ( "{\"tag\":\"PlayPath\",\"path\":\"smb-192.168.0.0-movies/path/to/file.mov\"}",
      ActionPlayPath $
        RawWebPath
          [ [twsQQ|smb-192.168.0.0-movies|],
            [twsQQ|path|],
            [twsQQ|to|],
            [twsQQ|file.mov|]
          ]
    )
  ]
