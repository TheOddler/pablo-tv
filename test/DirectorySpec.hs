module DirectorySpec (spec) where

import Autodocodec.Yaml (eitherDecodeYamlViaCodec, encodeYamlViaCodec)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Directory
import Path (fromRelFile, parseAbsDir, parseRelDir, parseRelFile)
import Test.Syd (Spec, describe, expectationFailure, it, pureGoldenByteStringFile, pureGoldenTextFile, shouldBe)
import TestUtils (forceAbsDir, forceRelFile)

spec :: Spec
spec = do
  describe "Correctly parses folders" $
    mapM_ folderSpec folderExamples

  it "Correctly cleans file names" $
    pureGoldenTextFile "test/golden/cleanFileName" $
      T.unlines
        [ T.unlines
            [ "Dir:   " <> niceDirNameT (forceAbsDir root),
              "Orig:  " <> T.pack (fromRelFile file),
              "Clean: " <> niceFileNameT file
            ]
          | (root, files, _dirs, _mPathAndInfo) <- folderExamples,
            file <- forceRelFile <$> files,
            isVideoFile file
        ]
  describe "Saving info files" $ do
    it "basic info" $
      pureGoldenByteStringFile "test/golden/basic-info.yaml" $
        encodeYamlViaCodec
          DirectoryInfo
            { directoryInfoKind = DirectoryKindSeries,
              directoryInfoTitle = "Pabloland",
              directoryInfoYear = Nothing,
              directoryInfoDescription = Nothing,
              directoryInfoImdb = Nothing,
              directoryInfoTvdb = Nothing,
              directoryInfoTmdb = Nothing,
              directoryInfoForceUpdate = Nothing
            }
    it "with description" $
      pureGoldenByteStringFile "test/golden/with-description.yaml" $
        encodeYamlViaCodec
          DirectoryInfo
            { directoryInfoKind = DirectoryKindSeries,
              directoryInfoTitle = "Pabloland",
              directoryInfoYear = Nothing,
              directoryInfoDescription = Just "Super interesting series with characters and stuff.",
              directoryInfoImdb = Nothing,
              directoryInfoTvdb = Nothing,
              directoryInfoTmdb = Nothing,
              directoryInfoForceUpdate = Nothing
            }
    it "all info" $
      pureGoldenByteStringFile "test/golden/all-info.yaml" $
        encodeYamlViaCodec
          DirectoryInfo
            { directoryInfoKind = DirectoryKindSeries,
              directoryInfoTitle = "Pabloland",
              directoryInfoYear = Just 1991,
              directoryInfoDescription = Just "A description",
              directoryInfoImdb = Just "tt1234567",
              directoryInfoTvdb = Just "123",
              directoryInfoTmdb = Just "abc",
              directoryInfoForceUpdate = Just True
            }

  it "Can decode info with missing fields" $ do
    let example =
          DirectoryInfo
            { directoryInfoKind = DirectoryKindMovie,
              directoryInfoTitle = "Master Movie",
              directoryInfoYear = Nothing,
              directoryInfoDescription = Nothing,
              directoryInfoImdb = Nothing,
              directoryInfoTvdb = Nothing,
              directoryInfoTmdb = Nothing,
              directoryInfoForceUpdate = Nothing
            }
    let encoded =
          BS.unlines
            [ "kind: movie",
              "title: Master Movie",
              "description: null"
            ]
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right x -> x `shouldBe` example

  it "Can decode info with fields in different order" $ do
    let example =
          DirectoryInfo
            { directoryInfoKind = DirectoryKindMovie,
              directoryInfoTitle = "Master Movie",
              directoryInfoYear = Nothing,
              directoryInfoDescription = Nothing,
              directoryInfoImdb = Nothing,
              directoryInfoTvdb = Nothing,
              directoryInfoTmdb = Nothing,
              directoryInfoForceUpdate = Nothing
            }
    let encoded =
          BS.unlines
            [ "description: null",
              "title: Master Movie",
              "kind: movie"
            ]
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right x -> x `shouldBe` example

  it "Can decode info with unknown fields" $ do
    let example =
          DirectoryInfo
            { directoryInfoKind = DirectoryKindMovie,
              directoryInfoTitle = "Master Movie",
              directoryInfoYear = Nothing,
              directoryInfoDescription = Nothing,
              directoryInfoImdb = Nothing,
              directoryInfoTvdb = Nothing,
              directoryInfoTmdb = Nothing,
              directoryInfoForceUpdate = Nothing
            }
    let encoded =
          BS.unlines
            [ "kind: movie",
              "title: Master Movie",
              "a-field-that-does-not-exit: value"
            ]
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right x -> x `shouldBe` example

  it "Can round-trip info file" $ do
    let example =
          DirectoryInfo
            { directoryInfoKind = DirectoryKindSeries,
              directoryInfoTitle = "Pabloland",
              directoryInfoYear = Just 1991,
              directoryInfoDescription = Just "A description",
              directoryInfoImdb = Just "tt1234567",
              directoryInfoTvdb = Just "123",
              directoryInfoTmdb = Just "abc",
              directoryInfoForceUpdate = Just True
            }
    let encoded = encodeYamlViaCodec example
    let decoded = eitherDecodeYamlViaCodec encoded
    case decoded of
      Left err -> expectationFailure $ show err
      Right x -> x `shouldBe` example

mkGuess :: DirectoryKind -> Text -> Maybe Int -> DirectoryInfo
mkGuess kind title year = DirectoryInfo kind title year Nothing Nothing Nothing Nothing Nothing

-- | Folder name, file names, expected results
folderExamples :: [(FilePath, [FilePath], [FilePath], DirectoryInfo)]
folderExamples =
  [ ( -- Base example
      "/Pabloland",
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
      mkGuess DirectoryKindSeries "Pabloland" Nothing
    ),
    ( -- A series without a season
      --  but the file names indicate it's a series, not a movie
      "/My TV Series",
      [ "Episode 6 - Episode Name (My TV Series) [abcd].mp4",
        "NEW My TV Series Episode Name [xyza].mp4"
      ],
      [],
      mkGuess DirectoryKindSeries "My TV Series" Nothing
    ),
    ( -- Flemish support
      "/Vlaamschen Serie (2021)/",
      [ "Vlaamschen Serie - Aflevering 2.avi",
        "Vlaamschen Serie - Aflevering 4.avi"
      ],
      [],
      mkGuess DirectoryKindSeries "Vlaamschen Serie" (Just 2021)
    ),
    ( -- Flemish support
      "/Vlaamschen Serie",
      [],
      [ "Seizoen 1",
        "Seizoen 2"
      ],
      mkGuess DirectoryKindSeries "Vlaamschen Serie" Nothing
    ),
    ( "/Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991).avi",
        "Film Van Mijn Jeugd (1991).srt"
      ],
      [],
      mkGuess DirectoryKindMovie "Film Van Mijn Jeugd" (Just 1991)
    ),
    ( "/Film Van Mijn Jeugd",
      [ "Film Van Mijn Jeugd (1991) Part 1.avi",
        "Film Van Mijn Jeugd (1991) Part 2.avi"
      ],
      [],
      mkGuess DirectoryKindMovie "Film Van Mijn Jeugd" (Just 1991)
    ),
    ( "/Film (2023)",
      [ "File.avi"
      ],
      [],
      mkGuess DirectoryKindMovie "Film" (Just 2023)
    ),
    ( "/Film",
      [ "File.avi"
      ],
      [],
      mkGuess DirectoryKindMovie "Film" Nothing
    ),
    ( "/Japanese Movie (2023, Dubbed)",
      [ "Japanese.Movie.2023.DUBBED.1080p.AMZN.WEBRip.DD5.1.x264-Woooops.mkv"
      ],
      [],
      mkGuess DirectoryKindMovie "Japanese Movie" (Just 2023)
    )
  ]

folderSpec :: (FilePath, [FilePath], [FilePath], DirectoryInfo) -> Spec
folderSpec (dirStr, fileNames, directoryNames, expected) = do
  it ("Correctly parses " <> dirStr) $ do
    dir <- parseAbsDir dirStr
    files <- mapM parseRelFile fileNames
    directories <- mapM parseRelDir directoryNames
    guessDirectoryInfo (DirectoryRaw dir files directories) `shouldBe` expected
