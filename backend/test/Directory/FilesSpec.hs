{-# LANGUAGE QuasiQuotes #-}

module Directory.FilesSpec where

import Directory.Files
import Test.Syd
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
  describe "nice names" $ do
    it "basic example" $
      niceFileNames
        [ VideoFileName [twsQQ|test.prefix a1lb and.suffix.mov|],
          VideoFileName [twsQQ|test.prefix a2mb and.suffix.avi|],
          VideoFileName [twsQQ|test.prefix a3nb and.suffix.mov|],
          VideoFileName [twsQQ|test.prefix a4ob and.suffix.mkv|]
        ]
        `shouldBe` NiceVideoFileNames
          "test prefix"
          [ "a1lb",
            "a2mb",
            "a3nb",
            "a4ob"
          ]
          "and suffix"
    it "removes delimiters" $
      niceFileNames
        [ VideoFileName [twsQQ|prefix.a1 suffix.mov|],
          VideoFileName [twsQQ|prefix a2.suffix.mov|]
        ]
        `shouldBe` NiceVideoFileNames
          "prefix"
          [ "a1",
            "a2"
          ]
          "suffix"
    it "strips the extension" $
      niceFileNames
        [ VideoFileName [twsQQ|prefix a1 suffix.mov|],
          -- Extensions are ignored, so add some different ones
          VideoFileName [twsQQ|prefix a2 suffix.avi|]
        ]
        `shouldBe` NiceVideoFileNames
          "prefix"
          [ "a1",
            "a2"
          ]
          "suffix"
    it "can handle a single file" $
      niceFileNames
        [ VideoFileName [twsQQ|prefix a1 suffix.mov|]
        ]
        `shouldBe` NiceVideoFileNames
          ""
          ["prefix a1 suffix"]
          ""
