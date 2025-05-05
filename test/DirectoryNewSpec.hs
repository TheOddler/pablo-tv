module DirectoryNewSpec (spec) where

import DirectoryNew
import DirectoryOld (DirectoryInfo (..), DirectoryKind (..))
import Orphanage ()
import System.Posix (getFileStatus)
import Test.Syd (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "Correctly reads test directory" $ do
    -- We don't actually check the fileStatus in the Eq instance in the Orphanage, so it doesn't matter, but we need something there and can't use `undefined` as I'm using `StrictData`.
    dummyFileStatus <- getFileStatus "test/directories/Videos/movie-a-v1/movie-a.mp4"
    dir <- readDirectory "test/directories/Videos"
    dir
      `shouldBe` Directory
        { directoryPath = "test/directories/Videos",
          directoryInfo = FileDoesNotExist,
          directoryWatched = FileDoesNotExist,
          directoryVideoFiles = [],
          directoryOtherFiles = [],
          directorySubDirs =
            [ Directory
                { directoryPath = "test/directories/Videos/movie-a-v1",
                  directoryInfo = FileDoesNotExist,
                  directoryWatched = FileDoesNotExist,
                  directoryVideoFiles =
                    [ VideoFile "test/directories/Videos/movie-a-v1/movie-a.mp4" dummyFileStatus
                    ],
                  directoryOtherFiles =
                    [ OtherFile "test/directories/Videos/movie-a-v1/movie-a.srt"
                    ],
                  directorySubDirs = []
                },
              Directory
                { directoryPath = "test/directories/Videos/movie-b-v1",
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
                    [ VideoFile "test/directories/Videos/movie-b-v1/movie-b.avi" dummyFileStatus
                    ],
                  directoryOtherFiles =
                    [ OtherFile "test/directories/Videos/movie-b-v1/movie-b.srt"
                    ],
                  directorySubDirs = []
                }
            ]
        }
