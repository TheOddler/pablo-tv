{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Util.DirPath
  ( DirPath (unDirPath),
    Abs,
    Rel,
    absPath,
    absPathQQ,
    relPath,
    relPathQQ,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q, Type (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Orphanage ()
import System.FilePath (isAbsolute, isRelative, isValid)

data Abs

data Rel

newtype DirPath b = UnsafeDirPath {unDirPath :: Text}
  deriving (Eq, Ord, Show)

absPath :: (MonadFail m) => Text -> m (DirPath Abs)
absPath = mkPathUnsafe "absolute" isAbsolute

relPath :: (MonadFail m) => Text -> m (DirPath Rel)
relPath = mkPathUnsafe "relative" isRelative

mkPathUnsafe :: (MonadFail m) => String -> (FilePath -> Bool) -> Text -> m (DirPath a)
mkPathUnsafe name checker t =
  let str = T.unpack t
   in if isValid str && checker str
        then pure $ UnsafeDirPath t
        else fail $ "Text is not a valid " ++ name ++ " file path"

absPathQQ :: QuasiQuoter
absPathQQ = mkPathQQUnsafe [t|Abs|] absPath

relPathQQ :: QuasiQuoter
relPathQQ = mkPathQQUnsafe [t|Rel|] relPath

mkPathQQUnsafe :: Q Type -> (Text -> Either String (DirPath a)) -> QuasiQuoter
mkPathQQUnsafe qType mkPath =
  QuasiQuoter
    { quoteExp = quoteAgeExp,
      quotePat = unsupported "patterns",
      quoteType = unsupported "types",
      quoteDec = unsupported "declarations"
    }
  where
    unsupported what _ = fail $ "quasiquoter does not support " ++ what
    quoteAgeExp :: String -> Q Exp
    quoteAgeExp str = do
      let txt = T.pack str
      case mkPath txt of
        Right _ -> do
          [|UnsafeDirPath @($qType) txt|]
        Left err -> fail err
