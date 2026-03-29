{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module DirectorySpec where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Directory (recursiveUpdateDirectoryNoSave)
import Directory.Directories
import Directory.Files
import Directory.Paths (DirectoryPath (..))
import Orphanage ()
import PVar (newPVar, readPVar)
import Samba (SmbServer (..), SmbShare (..))
import Test.Syd
import Test.Syd.Aeson
import TestUtils (runNoLogIO)
import UnliftIO.Directory (getCurrentDirectory)
import Util.DirPath (absPath, absPathQQ, relPathQQ)
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
  it "can decode file where I manually added a samba share" $ do
    (decoded :: Either String RootDirectories) <- eitherDecodeFileStrict "test/golden/manual-add-root.json"
    decoded `shouldSatisfy` isRight

  it "correctly en/decodes simple example" $ do
    pureGoldenJSONValueFile "test/golden/simple.json" $
      Map.fromList
        [ ( RootRelToHome [relPathQQ|Videos|],
            RootDirectoryData
              { rootDirectorySubDirs =
                  Map.fromList
                    [ ( DirectoryName [twsQQ|dir name|],
                        DirectoryData
                          { directoryImage = Nothing,
                            directorySubDirs =
                              Map.fromList
                                [ ( DirectoryName [twsQQ|sub dir|],
                                    DirectoryData
                                      { directoryImage = Nothing,
                                        directorySubDirs = Map.empty,
                                        directoryVideoFiles = Map.empty
                                      }
                                  )
                                ],
                            directoryVideoFiles =
                              Map.fromList
                                [ ( VideoFileName [twsQQ|video.avi|],
                                    VideoFileData
                                      { videoFileAdded = read "2000-01-01 01:01:01",
                                        videoFileWatched = Nothing
                                      }
                                  )
                                ]
                          }
                      )
                    ],
                rootDirectoryVideoFiles =
                  Map.fromList
                    [ ( VideoFileName [twsQQ|test.mov|],
                        VideoFileData
                          { videoFileAdded = read "2000-01-01 01:01:01",
                            videoFileWatched = Nothing
                          }
                      )
                    ]
              }
          ),
          ( RootSamba (SmbServer "192.168.0.23") (SmbShare "movies"),
            RootDirectoryData
              { rootDirectorySubDirs =
                  Map.empty,
                rootDirectoryVideoFiles =
                  Map.fromList
                    [ ( VideoFileName [twsQQ|video.mp4|],
                        VideoFileData
                          { videoFileAdded = read "2000-01-01 01:01:01",
                            videoFileWatched = Nothing
                          }
                      )
                    ]
              }
          ),
          ( RootRelToHome [relPathQQ|relative/to/home|],
            RootDirectoryData
              { rootDirectorySubDirs =
                  Map.empty,
                rootDirectoryVideoFiles =
                  Map.empty
              }
          ),
          ( RootAbsPath [absPathQQ|/absPath/to/home|],
            RootDirectoryData
              { rootDirectorySubDirs =
                  Map.empty,
                rootDirectoryVideoFiles =
                  Map.empty
              }
          )
        ]

  it "can read the example root" $ do
    curDir <- getCurrentDirectory
    let exampleRootsDir = curDir ++ "/test/Example Roots"
    root1Path <- absPath $ T.pack exampleRootsDir <> "/Root 1"
    let root1Location = RootAbsPath root1Path
    rootsPVar <-
      newPVar . Map.singleton root1Location $
        RootDirectoryData
          { rootDirectorySubDirs = Map.empty,
            rootDirectoryVideoFiles = Map.empty
          }
    let examplePath =
          DirectoryPath
            { directoryPathRoot = root1Location,
              directoryPathNames = []
            }
    runNoLogIO $ recursiveUpdateDirectoryNoSave rootsPVar examplePath
    updated <- readPVar rootsPVar
    let expected =
          [ ( root1Location,
              RootDirectoryData
                { rootDirectorySubDirs =
                    [ ( DirectoryName [twsQQ|A video|],
                        DirectoryData
                          { directoryImage = Just (ImageFileName [twsQQ|poster.jpg|], ImageFileData ""),
                            directorySubDirs = [],
                            directoryVideoFiles =
                              [ ( VideoFileName [twsQQ|video-file.mp4|],
                                  VideoFileData
                                    { videoFileAdded = read "2026-03-29 08:19:09.495531321 UTC",
                                      videoFileWatched = Nothing
                                    }
                                )
                              ]
                          }
                      ),
                      ( DirectoryName [twsQQ|With sub-folders|],
                        DirectoryData
                          { directoryImage = Just (ImageFileName [twsQQ|the-series-poster.png|], ImageFileData ""),
                            directorySubDirs =
                              [ ( DirectoryName [twsQQ|Sub 1|],
                                  DirectoryData
                                    { directoryImage = Just (ImageFileName [twsQQ|poster.webp|], ImageFileData ""),
                                      directorySubDirs = [],
                                      directoryVideoFiles =
                                        [ ( VideoFileName [twsQQ|File 1.1 - Test.mkv|],
                                            VideoFileData
                                              { videoFileAdded = read "2026-03-29 08:20:30.170967939 UTC",
                                                videoFileWatched = Nothing
                                              }
                                          ),
                                          ( VideoFileName [twsQQ|File 1.2 - Second.avi|],
                                            VideoFileData
                                              { videoFileAdded = read "2026-03-29 08:20:52.777086274 UTC",
                                                videoFileWatched = Nothing
                                              }
                                          )
                                        ]
                                    }
                                ),
                                ( DirectoryName [twsQQ|Sub 2|],
                                  DirectoryData
                                    { directoryImage = Nothing,
                                      directorySubDirs = [],
                                      directoryVideoFiles =
                                        [ ( VideoFileName [twsQQ|Video 2.mov|],
                                            VideoFileData
                                              { videoFileAdded = read "2026-03-29 08:22:20.267529609 UTC",
                                                videoFileWatched = Nothing
                                              }
                                          )
                                        ]
                                    }
                                )
                              ],
                            directoryVideoFiles = []
                          }
                      )
                    ],
                  rootDirectoryVideoFiles = []
                }
            )
          ]
    updated `shouldBe` expected
