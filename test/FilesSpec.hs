module FilesSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Files
import Path (File, Path, Rel, fromRelFile, parseRelDir, parseRelFile)
import Test.Syd (Spec, describe, it, pureGoldenTextFile, shouldBe)
import TestUtils (labeledExpectationFailure)

spec :: Spec
spec = do
  describe "Correctly parses folders" $
    mapM_ folderSpec folderExamples

  it "Correctly cleans file names" $
    pureGoldenTextFile "test/golden/cleanFileName" $
      T.unlines
        [ T.unlines
            [ "Dir:   " <> T.pack root,
              "Orig:  " <> T.pack (fromRelFile file),
              "Clean: " <> niceFileNameT file
            ]
          | (root, files, _dirs, _mPathAndInfo) <- folderExamples,
            file <- forceRelFile <$> files,
            isVideoFile file
        ]

forceRelFile :: FilePath -> Path Rel File
forceRelFile file =
  case parseRelFile file of
    Left err -> labeledExpectationFailure "Failed forceRelFile" err
    Right f -> f

mkGuess :: DirectoryKind -> Text -> Maybe Text -> DirectoryInfo
mkGuess kind title diff = DirectoryInfo kind title Nothing diff Nothing Nothing Nothing Nothing

-- | Folder name, file names, expected results
folderExamples :: [(FilePath, [FilePath], [FilePath], Maybe (FilePath, DirectoryInfo))]
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
      Just
        ( "Pabloland",
          mkGuess DirectoryKindSeries "Pabloland" Nothing
        )
    ),
    ( -- A series without a season
      --  but the file names indicate it's a series, not a movie
      "My TV Series",
      [ "Episode 6 - Episode Name (My TV Series) [abcd].mp4",
        "NEW My TV Series Episode Name [xyza].mp4"
      ],
      [],
      Just
        ( "My TV Series",
          mkGuess DirectoryKindSeries "My TV Series" Nothing
        )
    ),
    ( -- Flemish support
      "Vlaamschen Serie/Seizoen 1",
      [ "Vlaamschen Serie - Aflevering 2.avi",
        "Vlaamschen Serie - Aflevering 4.avi"
      ],
      [],
      Just
        ( "Vlaamschen Serie",
          mkGuess DirectoryKindSeries "Vlaamschen Serie" Nothing
        )
    ),
    ( -- Flemish support
      "Vlaamschen Serie",
      [],
      [ "Seizoen 1",
        "Seizoen 2"
      ],
      Just
        ( "Vlaamschen Serie",
          mkGuess DirectoryKindSeries "Vlaamschen Serie" Nothing
        )
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991).avi",
        "Film Van Mijn Jeugd (1991).srt"
      ],
      [],
      Just
        ( "Film Van Mijn Jeugd",
          mkGuess DirectoryKindMovie "Film Van Mijn Jeugd" (Just "1991")
        )
    ),
    ( "Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991) Part 1.avi",
        "Film Van Mijn Jeugd (1991) Part 2.avi"
      ],
      [],
      Just
        ( "Film Van Mijn Jeugd",
          mkGuess DirectoryKindMovie "Film Van Mijn Jeugd" (Just "1991")
        )
    ),
    ( "Film (2023)",
      [ "File.avi"
      ],
      [],
      Just
        ( "Film (2023)",
          mkGuess DirectoryKindMovie "Film" (Just "2023")
        )
    ),
    ( "Film",
      [ "File.avi"
      ],
      [],
      Just
        ( "Film",
          mkGuess DirectoryKindMovie "Film" Nothing
        )
    ),
    ( "Japanese Movie (2023, Dubbed)",
      [ "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv"
      ],
      [],
      Just
        ( "Japanese Movie (2023, Dubbed)",
          mkGuess DirectoryKindMovie "Japanese Movie" (Just "2023")
        )
    )
  ]

folderSpec :: (FilePath, [FilePath], [FilePath], Maybe (FilePath, DirectoryInfo)) -> Spec
folderSpec (dirStr, fileNames, directoryNames, expected) = do
  it ("Correctly parses " <> dirStr) $ do
    dir <- parseRelDir dirStr
    files <- mapM parseRelFile fileNames
    directories <- mapM parseRelDir directoryNames
    let mapExpected Nothing = Nothing
        mapExpected (Just (dirName, dirInfo)) = do
          mappedDir <- parseRelDir dirName
          Just (mappedDir, dirInfo)
    guessDirectoryInfo dir files directories `shouldBe` mapExpected expected
