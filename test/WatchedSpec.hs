module WatchedSpec (spec) where

import Autodocodec.Yaml (eitherDecodeYamlViaCodec, encodeYamlViaCodec)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Directory.Watched
import Path (File, Path, Rel)
import System.Posix.Types qualified as Posix
import Test.Syd (Spec, expectationFailure, it, pureGoldenByteStringFile, shouldBe)
import TestUtils (forceRelFile)

mkWF :: FilePath -> String -> Posix.FileID -> (Path Rel File, (UTCTime, Posix.FileID))
mkWF fileName timeStr fileId =
  ( forceRelFile fileName,
    (read timeStr, fileId)
  )

exampleInfo :: WatchedFiles
exampleInfo =
  WatchedFiles $
    Map.fromList
      [ mkWF "foo.mp4" "2021-01-01 00:00:00 UTC" 123,
        mkWF "bar.mp4" "2021-01-02 00:00:00 UTC" 123
      ]

spec :: Spec
spec = do
  it "writes the correct file format" $
    pureGoldenByteStringFile "test/golden/watched-info.yaml" $
      encodeYamlViaCodec exampleInfo

  it "Can round-trip watched info file" $ do
    let encoded = encodeYamlViaCodec exampleInfo
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right x -> x `shouldBe` exampleInfo

  it "Can handle slightly imperfect files" $ do
    let encoded =
          BS.unlines
            [ "file1.mp4: 2021-01-01 15:16:17 UTC; 123",
              "file2.mp4: 2021-01-01 15:16:17 UTC;   ",
              "file3.mp4:    2021-01-01   15:16:17  UTC;",
              "file4.mp4: 2021-01-01 15:16:17 UTC;",
              "file5.mp4: 2021-01-01 15:16:17 UTC",
              "file6.mp4: 2021-01-01 15:16:17",
              "file7.mp4: 2021-01-01 26:00:00",
              "file7.mp4:    2021-01-01 26:00:00",
              "file8.mp4: 2021-01-01"
            ]
    let expected =
          -- Use a list here, so we can detect missing entries, or in case we
          -- forgot to name each file uniquely, as internally it's a map.
          [ mkWF "file1.mp4" "2021-01-01 15:16:17 UTC" 123,
            mkWF "file2.mp4" "2021-01-01 15:16:17 UTC" 0,
            mkWF "file3.mp4" "2021-01-01 15:16:17 UTC" 0,
            mkWF "file4.mp4" "2021-01-01 15:16:17 UTC" 0,
            mkWF "file5.mp4" "2021-01-01 15:16:17 UTC" 0,
            mkWF "file6.mp4" "2021-01-01 15:16:17 UTC" 0,
            mkWF "file7.mp4" "2021-01-01 00:00:00 UTC" 0,
            mkWF "file8.mp4" "2021-01-01 00:00:00 UTC" 0
          ]
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right wfs -> Map.toList wfs `shouldBe` expected
