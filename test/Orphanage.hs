{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage where

import Actions (Action (..), DirOrFile (..), KeyboardButton (..), MouseButton (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Mpris qualified
import Path qualified
import Test.QuickCheck (Arbitrary (..), elements, listOf)
import Test.QuickCheck.Instances ()
import TestUtils (forcePath, genDirName, genDirRoot, genFileName)

deriving instance Generic Action

deriving instance Generic DirOrFile

deriving instance Generic MouseButton

deriving instance Generic KeyboardButton

deriving instance Generic Mpris.MprisAction

instance Arbitrary (Path.Path Path.Abs Path.Dir) where
  arbitrary = do
    root <- genDirRoot
    rest <- listOf genDirName
    pure $ foldl (Path.</>) root rest
  shrink a =
    let parent = Path.parent a
     in [parent | parent /= a]

instance Arbitrary (Path.Path Path.Abs Path.File) where
  arbitrary = do
    root <- arbitrary
    fileNameNoExt <- genFileName
    ext <- elements [".avi", ".mp4", ".mkv"]
    let fileName = forcePath $ Path.addExtension ext fileNameNoExt
    pure $ root Path.</> fileName
  shrink a = do
    dir <- shrink $ Path.parent a
    pure $ dir Path.</> Path.filename a

instance Arbitrary DirOrFile where
  arbitrary = genericArbitrary uniform

instance Arbitrary Action where
  arbitrary = genericArbitrary uniform

instance Arbitrary MouseButton where
  arbitrary = genericArbitrary uniform

instance Arbitrary KeyboardButton where
  arbitrary = genericArbitrary uniform

instance Arbitrary Mpris.MprisAction where
  arbitrary = genericArbitrary uniform
