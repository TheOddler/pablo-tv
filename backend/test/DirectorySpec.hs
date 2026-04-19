{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module DirectorySpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Directory (recursiveUpdateDirectoryNoSave, recursivelyUpdateAllDirectoriesNoSave)
import Directory.Directories
import Directory.Files
import Directory.Paths (DirectoryPath (..))
import Orphanage ()
import PVar (newPVar, readPVar)
import Path (toFilePath)
import Samba (SmbServer (..), SmbShare (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Posix.ByteString (RawFilePath, closeFd, createFile)
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Path (tempDirSpec)
import TestUtils (runTestIO, testCurrentTime, testModificationTime)
import UnliftIO.Directory (getCurrentDirectory)
import Util.DirPath (absPath, absPathQQ, relPathQQ)
import Util.TextWithoutSeparator (removeSeparatorsFromText, twsQQ)

someTestTime :: UTCTime
someTestTime =
  UTCTime
    (fromGregorian 2000 1 1)
    (secondsToDiffTime (1 * 3600 + 1 * 60 + 1))

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
                                      { videoFileAdded = someTestTime,
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
                          { videoFileAdded = someTestTime,
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
                          { videoFileAdded = someTestTime,
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

  describe "roots" rootSpec
  describe "broken files" brokenFileSpec

rootSpec :: Spec
rootSpec = do
  curDir <- getCurrentDirectory
  let exampleRootsDir = curDir ++ "/test/Example Roots"

  root1Location <- liftIO $ RootAbsPath <$> absPath (T.pack exampleRootsDir <> "/Root 1")
  let expectedRoot1 =
        RootDirectoryData
          { rootDirectorySubDirs =
              [ ( DirectoryName [twsQQ|A video|],
                  DirectoryData
                    { directoryImage = Just (ImageFileName [twsQQ|poster.jpg|], ImageFileData ""),
                      directorySubDirs = [],
                      directoryVideoFiles =
                        [ ( VideoFileName [twsQQ|video-file.mp4|],
                            VideoFileData
                              { videoFileAdded = testModificationTime,
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
                                        { videoFileAdded = testModificationTime,
                                          videoFileWatched = Nothing
                                        }
                                    ),
                                    ( VideoFileName [twsQQ|File 1.2 - Second.avi|],
                                      VideoFileData
                                        { videoFileAdded = testModificationTime,
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
                                        { videoFileAdded = testModificationTime,
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

  root2Location <- liftIO $ RootAbsPath <$> absPath (T.pack exampleRootsDir <> "/Root 2")
  let expectedRoot2 =
        RootDirectoryData
          { rootDirectorySubDirs =
              [ ( DirectoryName [twsQQ|My Folder|],
                  DirectoryData
                    { directoryImage = Nothing,
                      directorySubDirs = [],
                      directoryVideoFiles =
                        [ ( VideoFileName [twsQQ|my-home-video.mp4|],
                            VideoFileData
                              { videoFileAdded = testModificationTime,
                                videoFileWatched = Nothing
                              }
                          )
                        ]
                    }
                )
              ],
            rootDirectoryVideoFiles = []
          }

  let rootsInfo :: [(String, RootDirectoryLocation, RootDirectoryData)]
      rootsInfo =
        [ ("root 1", root1Location, expectedRoot1),
          ("root 2", root2Location, expectedRoot2)
        ]
  forM_ rootsInfo $ \(name, loc, expected) -> do
    it ("can read example " ++ name) $ runTestIO $ do
      rootsPVar <- newPVar [(loc, RootDirectoryData Map.empty Map.empty)]
      let examplePath = DirectoryPath loc []
      recursiveUpdateDirectoryNoSave rootsPVar examplePath
      updated <- readPVar rootsPVar
      liftIO $ updated `shouldBe` [(loc, expected)]

  it "can read all roots together" $ runTestIO $ do
    rootsPVar <-
      newPVar
        [ (root1Location, RootDirectoryData Map.empty Map.empty),
          (root2Location, RootDirectoryData Map.empty Map.empty)
        ]
    recursivelyUpdateAllDirectoriesNoSave rootsPVar
    updated <- readPVar rootsPVar
    let expected =
          [ (root1Location, expectedRoot1),
            (root2Location, expectedRoot2)
          ]
    liftIO $ updated `shouldBe` expected

brokenFileSpec :: Spec
brokenFileSpec = do
  tempDirSpec "invalid names" $
    it "correctly handles files with invalid names" $ \tempDirPath' -> do
      -- We cannot commit a file with an invalid character like this in git, so have to create the file (and a folder for it to live in) here
      let tempDirPath = toFilePath tempDirPath'
      let folderPath = tempDirPath </> "my test folder/"
      createDirectory folderPath
      let invalidFilePath :: RawFilePath
          -- \x80\xff is some invalid character
          invalidFilePath = BS8.pack folderPath <> "invalid_\x80\xff_file.mp4"
      fd <- createFile invalidFilePath 0o644
      closeFd fd

      -- Now the actual test
      runTestIO $ do
        rootLocation <- liftIO $ RootAbsPath <$> absPath (T.pack tempDirPath)
        rootsPVar <-
          newPVar
            [ (rootLocation, RootDirectoryData Map.empty Map.empty)
            ]
        recursivelyUpdateAllDirectoriesNoSave rootsPVar
        updated <- readPVar rootsPVar
        let expectedRoot =
              RootDirectoryData
                { rootDirectorySubDirs =
                    [ ( DirectoryName [twsQQ|my test folder|],
                        DirectoryData
                          { directoryImage = Nothing,
                            directorySubDirs = [],
                            directoryVideoFiles =
                              [ ( VideoFileName $ removeSeparatorsFromText "invalid_\65533\65533_file.mp4",
                                  VideoFileData
                                    { -- On broken file names we fall back to the current time, as we cannot read the mod time
                                      videoFileAdded = testCurrentTime,
                                      videoFileWatched = Nothing
                                    }
                                )
                              ]
                          }
                      )
                    ],
                  rootDirectoryVideoFiles = []
                }
        let expected =
              [ (rootLocation, expectedRoot)
              ]
        liftIO $ updated `shouldBe` expected
