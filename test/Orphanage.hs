{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Actions (Action (..), KeyboardButton (..), MouseButton (..))
import Data.HashSet qualified as Set
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Directory.Directories (DirectoryName (..), RootDirectoryLocation (..))
import Directory.Files (VideoFileName (..), videoFileExts)
import Directory.Paths (DirectoryPath (..), RawWebPath (..), VideoFilePath (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Mpris qualified
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (Arbitrary (..), elements, genericShrink, listOf1, suchThat, suchThatMap)
import Test.QuickCheck.Instances ()
import Util.AbsFilePath (AbsFilePath (..), absFilePath)
import Util.TextWithoutSeparator (TextWithoutSeparator (..), removeSeparatorsFromText, textWithoutSeparator, twsQQ)

instance Arbitrary TextWithoutSeparator where
  arbitrary = arbitrary `suchThatMap` textWithoutSeparator
  shrink a = mapMaybe textWithoutSeparator $ shrink $ unTextWithoutSeparator a

instance Arbitrary SmbServer where
  arbitrary = do
    a <- elements ['0' .. '9']
    b <- elements ['0' .. '9']
    pure $ SmbServer $ "192.168.0." ++ [a, b]
  shrink _ = []

instance Arbitrary SmbShare where
  arbitrary = do
    letters <- listOf1 $ elements ['a' .. 'z']
    pure $ SmbShare letters
  shrink x = SmbShare <$> shrink (unSmbShare x)

instance Arbitrary DirectoryName where
  arbitrary = DirectoryName <$> arbitrary
  shrink x = DirectoryName <$> shrink (unDirectoryName x)

instance Arbitrary AbsFilePath where
  arbitrary = arbitrary `suchThatMap` absFilePath
  shrink a = mapMaybe absFilePath $ shrink $ unAbsFilePath a

instance Arbitrary RootDirectoryLocation where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

deriving instance Generic RawWebPath

instance Arbitrary RawWebPath where
  -- We can't differentiate between [] and [""] here, and we don't really care either.
  -- So in the test, just don't generate [""] as we don't care for it.
  arbitrary = RawWebPath <$> arbitrary `suchThat` (/= [[twsQQ||]])
  shrink = filter (/= RawWebPath [[twsQQ||]]) . genericShrink

deriving instance Generic DirectoryPath

instance Arbitrary DirectoryPath where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary VideoFileName where
  arbitrary = do
    name <- arbitrary
    ext <- elements $ Set.toList videoFileExts
    pure . VideoFileName $ name <> ext
  shrink x =
    case T.breakOnEnd "." $ unTextWithoutSeparator $ unVideoFileName x of
      (name, "") -> VideoFileName . removeSeparatorsFromText <$> shrink name
      (name, ext') ->
        let ext = "." <> ext'
            mk n = VideoFileName . removeSeparatorsFromText $ n <> ext
         in mk <$> shrink name

deriving instance Generic VideoFilePath

instance Arbitrary VideoFilePath where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

deriving instance Generic Action

instance Arbitrary Action where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary MouseButton where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary KeyboardButton where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary Mpris.MprisAction where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink
