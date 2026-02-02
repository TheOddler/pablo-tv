{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Util.AbsFilePath
  ( AbsFilePath (unAbsFilePath),
    absFilePath,
    absFilePathQQ,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Orphanage ()
import System.FilePath (isAbsolute, isValid)

newtype AbsFilePath = UnsafeAbsFilePath {unAbsFilePath :: Text}
  deriving (Eq, Ord, Show)

absFilePath :: (MonadFail m) => Text -> m AbsFilePath
absFilePath t =
  let str = T.unpack t
   in if isValid str && isAbsolute str
        then pure $ UnsafeAbsFilePath t
        else fail "Text is not a valid absolute file path"

absFilePathQQ :: QuasiQuoter
absFilePathQQ =
  QuasiQuoter
    { quoteExp = quoteAgeExp,
      quotePat = unsupported "patterns",
      quoteType = unsupported "types",
      quoteDec = unsupported "declarations"
    }
  where
    unsupported what _ = fail $ "tws (TextWithoutSeparator) quasiquoter does not support " ++ what
    quoteAgeExp :: String -> Q Exp
    quoteAgeExp str = do
      let txt = T.pack str
      case absFilePath txt of
        Right _ -> [|UnsafeAbsFilePath txt|]
        Left err -> fail err
