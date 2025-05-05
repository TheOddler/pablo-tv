module DirectorySpec (spec) where

import Directory
import Directory.Files
import DirectoryOld (DirectoryInfo (..), DirectoryKind (..))
import Orphanage ()
import Path ((</>))
import Path.IO (getCurrentDir)
import System.Posix (getFileStatus)
import Test.Syd (Spec, it, shouldBe)
import TestUtils (forceRelDir, forceRelFile)

spec :: Spec
spec = do
  it "Correctly reads test directory" $ do
    -- We don't actually check the fileStatus in the Eq instance in the Orphanage, so it doesn't matter, but we need something there and can't use `undefined` as I'm using `StrictData`.
    dummyFileStatus <- getFileStatus "test/directories/Videos/movie-a-v1/movie-a.mp4"
    curDir <- getCurrentDir
    let mkAbsDir rel = curDir </> forceRelDir rel
    let mkAbsFile rel = curDir </> forceRelFile rel
    let mkVideoFile relPath = VideoFile (mkAbsFile relPath) dummyFileStatus
    let mkOtherFile relPath = OtherFile (mkAbsFile relPath)
    dir <- readDirectory $ mkAbsDir "test/directories/Videos"
    dir
      `shouldBe` Directory
        { directoryPath = mkAbsDir "test/directories/Videos",
          directoryInfo = FileDoesNotExist,
          directoryWatched = FileDoesNotExist,
          directoryVideoFiles = [],
          directoryOtherFiles = [],
          directorySubDirs =
            [ Directory
                { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies",
                  directoryInfo = FileDoesNotExist,
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles = [],
                  directoryOtherFiles = [],
                  directorySubDirs =
                    [ Directory
                        { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies/first-movie-v1",
                          directoryInfo = FileDoesNotExist,
                          directoryWatched = FileDoesNotExist,
                          directoryVideoFiles =
                            [ mkVideoFile "test/directories/Videos/Grouped Movies/first-movie-v1/first-movie.mkv"
                            ],
                          directoryOtherFiles = [],
                          directorySubDirs = []
                        },
                      Directory
                        { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies/second-movie-v1",
                          directoryInfo =
                            FileRead $
                              DirectoryInfo
                                { directoryInfoKind = DirectoryKindMovie,
                                  directoryInfoTitle = "Second Movie in a group",
                                  directoryInfoYear = Just 2025,
                                  directoryInfoDescription =
                                    Just "Test Movie in a group",
                                  directoryInfoImdb = Just "tt123",
                                  directoryInfoTvdb = Just "series-123",
                                  directoryInfoTmdb = Just "123",
                                  directoryInfoForceUpdate = Nothing
                                },
                          directoryWatched = FileDoesNotExist,
                          directoryVideoFiles =
                            [ mkVideoFile "test/directories/Videos/Grouped Movies/second-movie-v1/second-movie.avi"
                            ],
                          directoryOtherFiles =
                            [ mkOtherFile "test/directories/Videos/Grouped Movies/second-movie-v1/second-movie.srt"
                            ],
                          directorySubDirs = []
                        }
                    ]
                },
              Directory
                { directoryPath = mkAbsDir "test/directories/Videos/movie-a-v1",
                  directoryInfo = FileDoesNotExist,
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles =
                    [ mkVideoFile "test/directories/Videos/movie-a-v1/movie-a.mp4"
                    ],
                  directoryOtherFiles =
                    [ mkOtherFile "test/directories/Videos/movie-a-v1/movie-a.srt"
                    ],
                  directorySubDirs = []
                },
              Directory
                { directoryPath = mkAbsDir "test/directories/Videos/movie-b-v1",
                  directoryInfo =
                    FileRead $
                      DirectoryInfo
                        { directoryInfoKind = DirectoryKindMovie,
                          directoryInfoTitle = "Movie B",
                          directoryInfoYear = Just 2025,
                          directoryInfoDescription =
                            Just "Test Movie with info file (format V1)",
                          directoryInfoImdb = Just "tt123",
                          directoryInfoTvdb = Just "series-123",
                          directoryInfoTmdb = Just "123",
                          directoryInfoForceUpdate = Nothing
                        },
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles =
                    [ mkVideoFile "test/directories/Videos/movie-b-v1/movie-b.avi"
                    ],
                  directoryOtherFiles =
                    [ mkOtherFile "test/directories/Videos/movie-b-v1/movie-b.srt"
                    ],
                  directorySubDirs = []
                }
            ]
        }

  it "Correctly guesses new info for test directory" $ do
    -- We don't actually check the fileStatus in the Eq instance in the Orphanage, so it doesn't matter, but we need something there and can't use `undefined` as I'm using `StrictData`.
    dummyFileStatus <- getFileStatus "test/directories/Videos/movie-a-v1/movie-a.mp4"
    curDir <- getCurrentDir
    let mkAbsDir rel = curDir </> forceRelDir rel
    let mkAbsFile rel = curDir </> forceRelFile rel
    let mkVideoFile relPath = VideoFile (mkAbsFile relPath) dummyFileStatus
    let mkOtherFile relPath = OtherFile (mkAbsFile relPath)
    dir <- readDirectory $ mkAbsDir "test/directories/Videos"
    let updatedDir = guessMissingInfoRecursive dir
    updatedDir
      `shouldBe` Directory
        { directoryPath = mkAbsDir "test/directories/Videos",
          directoryInfo = FileDoesNotExist,
          directoryWatched = FileDoesNotExist,
          directoryVideoFiles = [],
          directoryOtherFiles = [],
          directorySubDirs =
            [ Directory
                { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies",
                  directoryInfo = FileDoesNotExist,
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles = [],
                  directoryOtherFiles = [],
                  directorySubDirs =
                    [ Directory
                        { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies/first-movie-v1",
                          directoryInfo =
                            FileDirty
                              DirectoryInfo
                                { directoryInfoKind = DirectoryKindMovie,
                                  directoryInfoTitle = "first-movie-v1",
                                  directoryInfoYear = Nothing,
                                  directoryInfoDescription = Nothing,
                                  directoryInfoImdb = Nothing,
                                  directoryInfoTvdb = Nothing,
                                  directoryInfoTmdb = Nothing,
                                  directoryInfoForceUpdate = Nothing
                                },
                          directoryWatched = FileDoesNotExist,
                          directoryVideoFiles =
                            [ mkVideoFile "test/directories/Videos/Grouped Movies/first-movie-v1/first-movie.mkv"
                            ],
                          directoryOtherFiles = [],
                          directorySubDirs = []
                        },
                      Directory
                        { directoryPath = mkAbsDir "test/directories/Videos/Grouped Movies/second-movie-v1",
                          directoryInfo =
                            FileRead $
                              DirectoryInfo
                                { directoryInfoKind = DirectoryKindMovie,
                                  directoryInfoTitle = "Second Movie in a group",
                                  directoryInfoYear = Just 2025,
                                  directoryInfoDescription =
                                    Just "Test Movie in a group",
                                  directoryInfoImdb = Just "tt123",
                                  directoryInfoTvdb = Just "series-123",
                                  directoryInfoTmdb = Just "123",
                                  directoryInfoForceUpdate = Nothing
                                },
                          directoryWatched = FileDoesNotExist,
                          directoryVideoFiles =
                            [ mkVideoFile "test/directories/Videos/Grouped Movies/second-movie-v1/second-movie.avi"
                            ],
                          directoryOtherFiles =
                            [ mkOtherFile "test/directories/Videos/Grouped Movies/second-movie-v1/second-movie.srt"
                            ],
                          directorySubDirs = []
                        }
                    ]
                },
              Directory
                { directoryPath = mkAbsDir "test/directories/Videos/movie-a-v1",
                  directoryInfo =
                    FileDirty
                      DirectoryInfo
                        { directoryInfoKind = DirectoryKindMovie,
                          directoryInfoTitle = "movie-a-v1",
                          directoryInfoYear = Nothing,
                          directoryInfoDescription = Nothing,
                          directoryInfoImdb = Nothing,
                          directoryInfoTvdb = Nothing,
                          directoryInfoTmdb = Nothing,
                          directoryInfoForceUpdate = Nothing
                        },
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles =
                    [ mkVideoFile "test/directories/Videos/movie-a-v1/movie-a.mp4"
                    ],
                  directoryOtherFiles =
                    [ mkOtherFile "test/directories/Videos/movie-a-v1/movie-a.srt"
                    ],
                  directorySubDirs = []
                },
              Directory
                { directoryPath = mkAbsDir "test/directories/Videos/movie-b-v1",
                  directoryInfo =
                    FileRead $
                      DirectoryInfo
                        { directoryInfoKind = DirectoryKindMovie,
                          directoryInfoTitle = "Movie B",
                          directoryInfoYear = Just 2025,
                          directoryInfoDescription =
                            Just "Test Movie with info file (format V1)",
                          directoryInfoImdb = Just "tt123",
                          directoryInfoTvdb = Just "series-123",
                          directoryInfoTmdb = Just "123",
                          directoryInfoForceUpdate = Nothing
                        },
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles =
                    [ mkVideoFile "test/directories/Videos/movie-b-v1/movie-b.avi"
                    ],
                  directoryOtherFiles =
                    [ mkOtherFile "test/directories/Videos/movie-b-v1/movie-b.srt"
                    ],
                  directorySubDirs = []
                }
            ]
        }
