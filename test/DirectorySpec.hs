{-# LANGUAGE QuasiQuotes #-}

module DirectorySpec where

import Actions
import Data.Aeson (eitherDecode, encode)
import Data.Map.Strict qualified as Map
import Directory
import Directory.Directories
import Directory.Files
import Orphanage ()
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (property)
import Test.QuickCheck.Instances ()
import Test.Syd
import Test.Syd.Aeson
import Util.TextWithoutSeparator (twsQQ)

spec :: Spec
spec = do
  it "can roundtrip JSON" $ property $ \(action :: Action) ->
    let encoded = encode action
     in eitherDecode encoded `shouldBe` Right action

  it "correctly en/decodes simple example" $ do
    pureGoldenJSONValueFile "test/golden/simple.json" $
      let example :: RootDirectories
          example =
            Map.fromList
              [ ( RootLocalVideos,
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
                )
              ]
       in example
