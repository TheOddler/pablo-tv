module FilesSpec (spec) where

import Data.Text (Text, unpack)
import Files
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Correctly parses folders" $
    mapM_ folderSpec folderExamples

-- | Folder name, file names, expected results
folderExamples :: [(Text, [Text], [VideoInfo])]
folderExamples =
  [ ( -- Base example
      "Season 2",
      [ "Pabloland.s02e04.hdtv.x264-tla.mp4",
        "Pabloland.s02e02.XviD-AFG.avi",
        "Pabloland.S02E06.hdtv.x264-tla.mp4",
        -- Season 0 is assumed to be a special, and put at the end of the list
        "Pabloland.S00E09.Pabloland.Christmas.720p.HDTV.x264-GOGOGO[series].mkv",
        -- It's possible to have multiple seasons in one folder, and then
        -- we assume the file name is correct
        "Pabloland [01x04] The Episode Name.mp4",
        "Pabloland [01x04] The Episode Name.srt"
      ],
      [ VideoInfoEpisode $ EpisodeInfo 1 (EpisodeNumber 4) False "Pabloland [01x04] The Episode Name.mp4",
        VideoInfoEpisode $ EpisodeInfo 2 (EpisodeNumber 2) False "Pabloland.s02e02.XviD-AFG.avi",
        VideoInfoEpisode $ EpisodeInfo 2 (EpisodeNumber 4) False "Pabloland.s02e04.hdtv.x264-tla.mp4",
        VideoInfoEpisode $ EpisodeInfo 2 (EpisodeNumber 6) False "Pabloland.S02E06.hdtv.x264-tla.mp4",
        VideoInfoEpisode $ EpisodeInfo 2 (EpisodeNumber 9) True "Pabloland.S00E09.Pabloland.Christmas.720p.HDTV.x264-GOGOGO[series].mkv"
      ]
    ),
    ( -- A series without a season
      --  but the file names indicate it's a series, not a movie
      "My TV Series",
      [ "Episode 6 - Episode Name (My TV Series) [abcd].mp4",
        "NEW My TV Series Episode Name [xyza].mp4"
      ],
      [ VideoInfoEpisode $ EpisodeInfo 1 (EpisodeNumber 2) False "NEW My TV Series Episode Name [xyza].mp4",
        VideoInfoEpisode $ EpisodeInfo 1 (EpisodeNumber 6) False "Episode 6 - Episode Name (My TV Series) [abcd].mp4"
      ]
    ),
    ( -- Flemish support
      "Seizoen 1",
      [ "Vlaamschen Serie - Aflevering 2.avi",
        "Vlaamschen Serie - Aflevering 4.avi"
      ],
      [ VideoInfoEpisode $ EpisodeInfo 1 (EpisodeNumber 2) False "Vlaamschen Serie - Aflevering 2.avi",
        VideoInfoEpisode $ EpisodeInfo 1 (EpisodeNumber 4) False "Vlaamschen Serie - Aflevering 4.avi"
      ]
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991).avi",
        "Film Van Mijn Jeugd (1991).srt"
      ],
      [ VideoInfoMovie $ MovieInfo "Film Van Mijn Jeugd" (Just 1991) "Film Van Mijn Jeugd (1991).avi"
      ]
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991) Part 1.avi",
        "Film Van Mijn Jeugd (1991) Part 2.avi"
      ],
      [ VideoInfoMovie $ MovieInfo "Film Van Mijn Jeugd" (Just 1991) "Film Van Mijn Jeugd (1991) Part 1.avi",
        VideoInfoMovie $ MovieInfo "Film Van Mijn Jeugd" (Just 1991) "Film Van Mijn Jeugd (1991) Part 2.avi"
      ]
    ),
    ( "Japanese Movie (2023, Dubbed)",
      [ "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv"
      ],
      [ VideoInfoMovie $ MovieInfo "Japanese Movie" (Just 2023) "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv"
      ]
    )
  ]

folderSpec :: (Text, [Text], [VideoInfo]) -> Spec
folderSpec (folderName, fileNames, expected) =
  it ("Correctly parses " <> unpack folderName) $
    parseDirectory folderName fileNames `shouldBe` expected
