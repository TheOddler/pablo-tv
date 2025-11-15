{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Actions (Action (..), DirOrFile (..), KeyboardButton (..), MouseButton (..))
import Data.Text as T
import Directory (DirectoryName (..), DirectoryPath, RootDirectoryLocation, VideoFileName (..), VideoFilePath, videoFileExts)
import Generic.Random (genericArbitrary, uniform)
import Mpris qualified
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (Arbitrary (..), elements, listOf1)
import Test.QuickCheck.Instances ()
import TestUtils (genFileNameWithExt)

instance Arbitrary SmbServer where
  arbitrary = do
    a <- elements ['0' .. '9']
    b <- elements ['0' .. '9']
    pure $ SmbServer $ "192.168.0." ++ [a, b]

instance Arbitrary SmbShare where
  arbitrary = do
    letters <- listOf1 $ elements ['a' .. 'z']
    pure $ SmbShare letters

instance Arbitrary DirectoryName where
  arbitrary = do
    letters <- listOf1 $ elements ['a' .. 'z']
    pure $ DirectoryName $ T.pack letters

instance Arbitrary RootDirectoryLocation where
  arbitrary = genericArbitrary uniform

instance Arbitrary DirectoryPath where
  arbitrary = genericArbitrary uniform

instance Arbitrary VideoFileName where
  arbitrary = VideoFileName <$> genFileNameWithExt videoFileExts

instance Arbitrary VideoFilePath where
  arbitrary = genericArbitrary uniform

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
