module ElmHelpers where

import Data.Proxy (Proxy (..))
import Elm.Derive
import Elm.TyRep
import JSON
import Language.Haskell.TH

deriveElmPrefixed :: Name -> Q [Dec]
deriveElmPrefixed name = do
  prefix <- getPrefix name
  deriveElmDef (ourAesonOptionsPrefix prefix) name

-- | The ETypeDef for something that's encoded as a string alias
eTypeDefStringAlias :: String -> ETypeDef
eTypeDefStringAlias name =
  ETypePrimAlias $
    EPrimAlias
      (ETypeName name [])
      (toElmType (Proxy :: Proxy String))

eTypeDict :: String -> String -> EType
eTypeDict k v =
  ETyApp
    ( ETyApp
        (ETyCon $ ETCon "Dict")
        (ETyCon $ ETCon k)
    )
    (ETyCon $ ETCon v)
