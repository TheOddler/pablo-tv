{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage where

import Actions (Action (..), DirOrFile (..), KeyboardButton (..), MouseButton (..))
import Directory (Directory (..))
import Directory.Files (Image (..), OtherFile (..), SpecialFile (..), VideoFile (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Path qualified
import Playerctl qualified
import Test.QuickCheck (Arbitrary (..), elements, listOf)
import Test.QuickCheck.Instances ()
import TestUtils (forcePath, genDirName, genDirRoot, genFileName)

deriving instance Generic Action

deriving instance Generic DirOrFile

deriving instance Generic MouseButton

deriving instance Generic KeyboardButton

deriving instance Generic Playerctl.Action

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

instance Arbitrary Playerctl.Action where
  arbitrary = genericArbitrary uniform

-- Instances for the Directory tests
deriving instance Eq Directory

deriving instance Show Directory

deriving instance Eq Image

deriving instance Show Image

instance Eq VideoFile where
  VideoFile a _ == VideoFile b _ = a == b

instance Show VideoFile where
  -- This is just for testing, so not a great instance. We can't easily show
  -- the FileStatus but that doesn't matter for the tests really.
  show (VideoFile a _) = show a

deriving instance Eq OtherFile

deriving instance Show OtherFile

instance (Eq a) => Eq (SpecialFile a) where
  FileDoesNotExist == FileDoesNotExist = True
  FileRead a == FileRead b = a == b
  FileDirty a == FileDirty b = a == b
  FileReadFail bsA eA == FileReadFail bsB eB = bsA == bsB && show eA == show eB
  FileReadError a == FileReadError b = show a == show b
  _ == _ = False

deriving instance (Show a) => Show (SpecialFile a)
