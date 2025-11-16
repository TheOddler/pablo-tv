{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Actions (Action (..), DirOrFile (..), KeyboardButton (..), MouseButton (..))
import Data.HashSet qualified as Set
import Directory.Directories (DirectoryName (..), RootDirectoryLocation)
import Directory.Files (VideoFileName (..), videoFileExts)
import Directory.Paths (DirectoryPath (..), VideoFilePath (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Mpris qualified
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (Arbitrary (..), elements, listOf1, suchThatMap)
import Test.QuickCheck.Instances ()
import Util.TextWithoutSeparator (TextWithoutSeparator, textWithoutSeparator)

instance Arbitrary TextWithoutSeparator where
  arbitrary = arbitrary `suchThatMap` textWithoutSeparator

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
  arbitrary = DirectoryName <$> arbitrary

instance Arbitrary RootDirectoryLocation where
  arbitrary = genericArbitrary uniform

deriving instance Generic DirectoryPath

instance Arbitrary DirectoryPath where
  arbitrary = genericArbitrary uniform

instance Arbitrary VideoFileName where
  arbitrary = do
    name <- arbitrary
    ext <- elements $ Set.toList videoFileExts
    pure . VideoFileName $ name <> ext

deriving instance Generic VideoFilePath

instance Arbitrary VideoFilePath where
  arbitrary = genericArbitrary uniform

deriving instance Generic DirOrFile

instance Arbitrary DirOrFile where
  arbitrary = genericArbitrary uniform

deriving instance Generic Action

instance Arbitrary Action where
  arbitrary = genericArbitrary uniform

instance Arbitrary MouseButton where
  arbitrary = genericArbitrary uniform

instance Arbitrary KeyboardButton where
  arbitrary = genericArbitrary uniform

instance Arbitrary Mpris.MprisAction where
  arbitrary = genericArbitrary uniform
