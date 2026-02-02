{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Actions (Action (..), DirOrFile (..), KeyboardButton (..), MouseButton (..))
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Directory.Directories (DirectoryName (..), RootDirectoryLocation (..))
import Directory.Files (VideoFileName (..), videoFileExts)
import Directory.Paths (DirectoryPath (..), VideoFilePath (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Mpris qualified
import Samba (SmbServer (..), SmbShare (..))
import Test.QuickCheck (Arbitrary (..), elements, genericShrink, listOf1, suchThatMap)
import Util.AbsFilePath (AbsFilePath (..), absFilePath)
import Util.TextWithoutSeparator (TextWithoutSeparator (..), textWithoutSeparator, twsQQ)
import Util.TextWithoutSeparator qualified as TWS

instance Arbitrary T.Text where
  arbitrary =
    arbitrary `suchThatMap` \str ->
      -- In tests we don't generate strings with NUL in it, as we don't really want to bother supporting that anyway
      if '\NUL' `elem` str
        then Nothing
        else Just $ T.pack str
  shrink xs = T.pack <$> shrink (T.unpack xs)

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

-- shrink = \case
--   RootSamba _ _ -> []
--   RootLocalVideos -> []
--   RootAbsPath a -> RootAbsPath <$> shrink a

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
    case TWS.splitNE (== '.') $ unVideoFileName x of
      a NE.:| [] -> VideoFileName <$> shrink a
      parts ->
        let ext = NE.last parts
            name :: TextWithoutSeparator
            name = foldl (<>) [twsQQ||] $ NE.init parts
            mk n = VideoFileName $ n <> ext
         in mk <$> shrink name

deriving instance Generic VideoFilePath

instance Arbitrary VideoFilePath where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

deriving instance Generic DirOrFile

instance Arbitrary DirOrFile where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

deriving instance Generic Action

instance Arbitrary Scientific.Scientific where
  arbitrary = do
    Scientific.scientific <$> arbitrary <*> arbitrary
  shrink s =
    map (uncurry Scientific.scientific) $
      shrink (Scientific.coefficient s, Scientific.base10Exponent s)

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
