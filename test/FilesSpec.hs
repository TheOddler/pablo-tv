module FilesSpec (spec) where

import Data.Text (Text, pack, splitOn)
import Files
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Correctly parses episodes" $
    mapM_ fileNameSpec $
      mapFourth FileEpisode <$> episodeExamples

  describe "Correctly parses movies" $
    mapM_ fileNameSpec $
      mapFourth FileMovie <$> movieExamples

mapFourth :: (d -> e) -> (a, b, c, d) -> (a, b, c, e)
mapFourth f (a, b, c, d) = (a, b, c, f d)

episodeExamples :: [(Int, Int, String, EpisodeInfo)]
episodeExamples =
  [ ( 3,
      6,
      "Pabloland/Season 2/Pabloland.s02e04.hdtv.x264-tla.mp4",
      EpisodeInfo 2 (EpisodeNumber 4) False
    ),
    ( 3,
      6,
      "Pabloland/Season 2/Pabloland.s02e02.XviD-AFG.avi",
      EpisodeInfo 2 (EpisodeNumber 2) False
    ),
    ( 999,
      20,
      "Pabloland/Season 2/Pabloland.S02E06.hdtv.x264-tla.mp4",
      EpisodeInfo 2 (EpisodeNumber 6) False
    ),
    ( 7,
      7,
      "Pabloland/Season 1/Pabloland [01x04] The Episode Name.mp4",
      EpisodeInfo 1 (EpisodeNumber 4) False
    ),
    ( 8,
      10,
      "Pabloland/Season 3/Pabloland.S03E09.Pabloland.Christmas.720p.HDTV.x264-GOGOGO[series].mkv",
      EpisodeInfo 3 (EpisodeNumber 9) False
    ),
    ( 999,
      10,
      "My TV Series/Episode 6 - Episode Name (My TV Series) [abcd].mp4",
      EpisodeInfo 1 (EpisodeNumber 6) False
    ),
    ( 2,
      4,
      "My TV Series/NEW My TV Series Episode Name [xyza].mp4",
      EpisodeInfo 1 (EpisodeNumber 3) True
    ),
    ( 0,
      8,
      "Happy/Season 2/happy.s02e04.720p.hdtv.x264-tla.mp4",
      EpisodeInfo 2 (EpisodeNumber 4) False
    ),
    ( 0,
      6,
      "The Pablos/The Pablo s1e5.mp4",
      EpisodeInfo 1 (EpisodeNumber 5) False
    ),
    ( 1,
      8,
      "SeriesName/Season 4/SeriesName, s4e5, EpName.mp4",
      EpisodeInfo 4 (EpisodeNumber 5) False
    ),
    ( 1,
      3,
      "SeriesName/Season 4/SeriesName, Special s0e13, Christmas Special, Special Name.mp4",
      EpisodeInfo 4 (EpisodeNumber 13) True
    ),
    ( 1,
      5,
      "SeriesName/Season 4/SeriesName, s0e13, Its The Name.mp4",
      EpisodeInfo 4 (EpisodeNumber 13) True
    ),
    ( 1,
      5,
      "Good Series/Season 1/Good.Series.S01E01.720p.WEBRiP.x264-ABCD[pppp].mkv",
      EpisodeInfo 1 (EpisodeNumber 1) False
    ),
    ( 1,
      5,
      "Streaming Stuff-1 (1994)/Season 4/Streaming Stuff-1 - S04E05 - Binge It All (1080p x265 YEAR2020).mkv",
      EpisodeInfo 4 (EpisodeNumber 5) False
    ),
    ( 1,
      5,
      "Streaming Stuff-1 (1994)/Season 1/Streaming Stuff-1 - S01E01-E02 - Watching It All (1080p x265 YEAR2020).mkv",
      EpisodeInfo 1 (EpisodeNumberDouble 1 2) False
    ),
    ( 1,
      5,
      "Streaming Stuff-1 (1994)/Season 1/Streaming Stuff-1 - S00E01-E02 - Another Day Lost (1080p x265 YEAR2020).mkv",
      EpisodeInfo 1 (EpisodeNumberDouble 1 2) True
    ),
    ( 1,
      5,
      "Bum Bam (Good Animation)/Alternate Takes/Bum Bam Season 1 - From here to There.mp4",
      EpisodeInfo 1 (EpisodeNumber 2) False
    ),
    ( 1,
      5,
      "Bum Bam (Good Animation)/Season 2/Bum Bam S02E25 Small House....mp4",
      EpisodeInfo 2 (EpisodeNumber 25) False
    ),
    ( 1,
      5,
      "Bum Bam (Good Animation)/VA Commentary/Bum Bam Episode S02E02 - Commentary.mp4",
      EpisodeInfo 2 (EpisodeNumber 2) False
    ),
    ( 1,
      5,
      "Nukes/Nukes s1e4.mp4",
      EpisodeInfo 1 (EpisodeNumber 4) False
    ),
    ( 1,
      5,
      "Nukes/Nukes s1e6.mp4",
      EpisodeInfo 1 (EpisodeNumber 6) False
    ),
    ( 1,
      5,
      "Vlaamschen Serie/Season 1/Vlaamschen Serie - Aflevering 2.avi",
      EpisodeInfo 1 (EpisodeNumber 2) False
    ),
    ( 1,
      5,
      "Vlaamschen Serie/Season 1/Vlaamschen Serie - Aflevering 4.avi",
      EpisodeInfo 1 (EpisodeNumber 4) False
    ),
    ( 1,
      5,
      "British Series/Season 2/British Series - S02E03 - EpNaMe - [WEBDL-1080p h265].mkv",
      EpisodeInfo 2 (EpisodeNumber 3) False
    ),
    ( 1,
      5,
      "Sad Politics/Series 4/Sad Politics, Series 4, Episode 5 [bombam].mp4",
      EpisodeInfo 4 (EpisodeNumber 5) False
    ),
    ( 1,
      5,
      "Sad Politics/Series 2/Sad Politics, Series 2 Special 1: Ugh ffs [123456].mp4",
      EpisodeInfo 2 (EpisodeNumber 1) True
    ),
    ( 1,
      5,
      "I Like Dinos/I.Like.Dinos.2022.S01E01.1080p.x265-QWER.mkv",
      EpisodeInfo 1 (EpisodeNumber 1) False
    ),
    ( 1,
      5,
      "Name Of Series/Season 2/name.of.series.s02e06.720p.hdtv.x264-tla.mkv",
      EpisodeInfo 2 (EpisodeNumber 6) False
    )
  ]

movieExamples :: [(Int, Int, String, MovieInfo)]
movieExamples =
  [ ( 1,
      1,
      "Film Van Mijn Jeugd/Film Van Mijn Jeugd.mkv",
      MovieInfo "Film Van Mijn Jeugd" Nothing
    ),
    ( 1,
      1,
      "Film Van Mijn Jeugd/Film Van Mijn Jeugd Part 2.webm",
      MovieInfo "Film Van Mijn Jeugd" Nothing
    ),
    ( 1,
      1,
      "Film Van Mijn Jeugd/Film Van Mijn Jeugd Part 1.webm",
      MovieInfo "Film Van Mijn Jeugd" Nothing
    ),
    ( 1,
      1,
      "Film Van Mijn Jeugd/Film Van Mijn Jeugd.webm",
      MovieInfo "Film Van Mijn Jeugd" Nothing
    ),
    ( 1,
      1,
      "Tweede In De Reeks/Tweede.In.De.Reeks.2044.1080p.WEBRip.1600MB.DD5.1.x264-Goooooo.mkv",
      MovieInfo "Tweede In De Reeks" (Just 2044)
    ),
    ( 1,
      1,
      "Eerste In De Reeks/Eerste.In.De.Reeks.2016.1080p.HDR.Bluray.AV1.Multi4-smartperson.mkv",
      MovieInfo "Eerste In De Reeks" (Just 2016)
    ),
    ( 1,
      1,
      "Japanese Movie (2023, Dubbed)/Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv",
      MovieInfo "Japanese Movie" (Just 2023)
    ),
    ( 1,
      1,
      "Parent Parent/Parent (1991)/Actual File Here (1991).avi",
      MovieInfo "Parent" (Just 1991)
    )
  ]

fileNameSpec :: (Int, Int, String, FileInfo) -> Spec
fileNameSpec (indexInDir, fileCount, fileName, expected) =
  it ("Correctly parses " <> fileName) $ do
    let segments :: [Text]
        segments = splitOn "/" $ pack fileName

    parseFileName indexInDir fileCount segments `shouldBe` expected
