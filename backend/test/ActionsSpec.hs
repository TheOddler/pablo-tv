{-# LANGUAGE QuasiQuotes #-}

module ActionsSpec (spec) where

import Actions
import Data.Aeson (eitherDecode, encode)
import Directory.Paths (RawWebPath (..))
import Orphanage ()
import Test.QuickCheck (property, withMaxSuccess)
import Test.Syd
import Test.Syd.Aeson (pureGoldenJSONValueFile)
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
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
