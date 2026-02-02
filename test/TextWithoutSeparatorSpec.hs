{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module TextWithoutSeparatorSpec where

import Data.Foldable (forM_)
import Data.Text qualified as T
import Orphanage ()
import Test.QuickCheck
import Test.Syd
import Util.TextWithoutSeparator

spec :: Spec
spec = do
  let roundtrip replacement = unReplaceSeparatorWith replacement . replaceSeparatorWith replacement
  it "can roundtrip replaceSeparatorWith" $ property $ \(text :: T.Text, replacement :: TextWithoutSeparator) ->
    let roundtrip' = roundtrip replacement
     in roundtrip' text `shouldBe` text

  let exampleReplacement = [twsQQ|\|]
      examples =
        [ ("test", [twsQQ|test|]),
          ("test/test", [twsQQ|test\test|]),
          ("test//test", [twsQQ|test\\test|]),
          ("test\\test", [twsQQ|test\\test|])
        ]
  forM_ examples $ \(input, expected) ->
    it
      ( "correctly replaces separator with "
          ++ T.unpack (unTextWithoutSeparator exampleReplacement)
          ++ " in example "
          ++ T.unpack input
          ++ " as "
          ++ T.unpack (unTextWithoutSeparator expected)
      )
      $ do
        replaceSeparatorWith exampleReplacement input `shouldBe` expected
