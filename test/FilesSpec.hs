module FilesSpec (spec) where

import Data.Text (Text, pack, splitOn)
import Files
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Correctly parses episodes" $
    mapM_ fileNameSpec $
      mapThird FileEpisode <$> episodeExamples

  describe "Correctly parses movies" $
    mapM_ fileNameSpec $
      mapThird FileMovie <$> movieExamples

mapThird :: (c -> d) -> (a, b, c) -> (a, b, d)
mapThird f (a, b, c) = (a, b, f c)

episodeExamples :: [(Int, String, EpisodeInfo)]
episodeExamples = []

movieExamples :: [(Int, String, MovieInfo)]
movieExamples = []

fileNameSpec :: (Int, String, FileInfo) -> Spec
fileNameSpec (indexInDir, fileName, expected) =
  it ("Correctly parses " <> fileName) $ do
    let segments :: [Text]
        segments = splitOn "/" $ pack fileName

    parseFileName indexInDir segments `shouldBe` expected
