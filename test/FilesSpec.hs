{-# LANGUAGE TemplateHaskell #-}

module FilesSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Files
import Path (mkRelFile, parseRelDir, parseRelFile)
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Correctly parses folders" $
    mapM_ folderSpec folderExamples

-- | Folder name, file names, expected results
folderExamples :: [(FilePath, [FilePath], [FilePath], Maybe DirectoryInfo)]
folderExamples =
  [ ( -- Base example
      "Pabloland/Season 2",
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
      [],
      Just $
        SeasonDirectory "Pabloland" $
          NE.fromList
            [ EpisodeInfo 1 (EpisodeNumber 4) False $(mkRelFile "Pabloland [01x04] The Episode Name.mp4"),
              EpisodeInfo 2 (EpisodeNumber 2) False $(mkRelFile "Pabloland.s02e02.XviD-AFG.avi"),
              EpisodeInfo 2 (EpisodeNumber 4) False $(mkRelFile "Pabloland.s02e04.hdtv.x264-tla.mp4"),
              EpisodeInfo 2 (EpisodeNumber 6) False $(mkRelFile "Pabloland.S02E06.hdtv.x264-tla.mp4"),
              EpisodeInfo 2 (EpisodeNumber 9) True $(mkRelFile "Pabloland.S00E09.Pabloland.Christmas.720p.HDTV.x264-GOGOGO[series].mkv")
            ]
    ),
    ( -- A series without a season
      --  but the file names indicate it's a series, not a movie
      "My TV Series",
      [ "Episode 6 - Episode Name (My TV Series) [abcd].mp4",
        "NEW My TV Series Episode Name [xyza].mp4"
      ],
      [],
      Just $
        SeasonDirectory "My TV Series" $
          NE.fromList
            [ EpisodeInfo 1 (EpisodeNumber 2) False $(mkRelFile "NEW My TV Series Episode Name [xyza].mp4"),
              EpisodeInfo 1 (EpisodeNumber 6) False $(mkRelFile "Episode 6 - Episode Name (My TV Series) [abcd].mp4")
            ]
    ),
    ( -- Flemish support
      "Vlaamschen Serie/Seizoen 1",
      [ "Vlaamschen Serie - Aflevering 2.avi",
        "Vlaamschen Serie - Aflevering 4.avi"
      ],
      [],
      Just $
        SeasonDirectory "Vlaamschen Serie" $
          NE.fromList
            [ EpisodeInfo 1 (EpisodeNumber 2) False $(mkRelFile "Vlaamschen Serie - Aflevering 2.avi"),
              EpisodeInfo 1 (EpisodeNumber 4) False $(mkRelFile "Vlaamschen Serie - Aflevering 4.avi")
            ]
    ),
    ( -- Flemish support
      "Vlaamschen Serie",
      [],
      [ "Seizoen 1",
        "Seizoen 2"
      ],
      Just $
        SeriesDirectory "Vlaamschen Serie"
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991).avi",
        "Film Van Mijn Jeugd (1991).srt"
      ],
      [],
      Just
        $ MovieDirectory
          "Film Van Mijn Jeugd"
          (Just 1991)
        $ NE.fromList
          [ MovieInfo $(mkRelFile "Film Van Mijn Jeugd (1991).avi")
          ]
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991) Part 1.avi",
        "Film Van Mijn Jeugd (1991) Part 2.avi"
      ],
      [],
      Just $
        MovieDirectory "Film Van Mijn Jeugd" (Just 1991) $
          NE.fromList
            [ MovieInfo $(mkRelFile "Film Van Mijn Jeugd (1991) Part 1.avi"),
              MovieInfo $(mkRelFile "Film Van Mijn Jeugd (1991) Part 2.avi")
            ]
    ),
    ( "Film (2023)",
      [ "File.avi"
      ],
      [],
      Just $
        MovieDirectory "Film" (Just 2023) $
          NE.fromList
            [ MovieInfo $(mkRelFile "File.avi")
            ]
    ),
    ( "Film",
      [ "File.avi"
      ],
      [],
      Just $
        MovieDirectory "Film" Nothing $
          NE.fromList
            [ MovieInfo $(mkRelFile "File.avi")
            ]
    ),
    ( "Japanese Movie (2023, Dubbed)",
      [ "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv"
      ],
      [],
      Just $
        MovieDirectory "Japanese Movie" (Just 2023) $
          NE.fromList
            [ MovieInfo $(mkRelFile "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv")
            ]
    )
  ]

folderSpec :: (FilePath, [FilePath], [FilePath], Maybe DirectoryInfo) -> Spec
folderSpec (dirStr, fileNames, directoryNames, expected) = do
  it ("Correctly parses " <> dirStr) $ do
    dir <- parseRelDir dirStr
    files <- mapM parseRelFile fileNames
    directories <- mapM parseRelDir directoryNames
    parseDirectory' dir files directories `shouldBe` expected
