{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module JSON where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveFromJSON, deriveJSON, deriveToJSON)
import Data.Char qualified as Char
import Data.List.Extra (dropPrefix)
import GHC.TypeLits (Symbol)
import Language.Haskell.TH (Dec (..), Info (..), Name, Q, TyLit (..), TySynEqn (..), Type (..), isInstance, reify)

ourAesonOptionsPrefix :: String -> Aeson.Options
ourAesonOptionsPrefix prefix =
  Aeson.defaultOptions
    { -- Aeson.sumEncoding = Aeson.ObjectWithSingleField,
      Aeson.unwrapUnaryRecords = True,
      Aeson.omitNothingFields = True,
      Aeson.allNullaryToStringTag = True,
      Aeson.constructorTagModifier = dropPrefix constructorPrefix,
      Aeson.fieldLabelModifier =
        mapFirstLetter Char.toLower . dropPrefix fieldPrefix
    }
  where
    mapFirstLetter mapper = \case
      "" -> ""
      (firstLetter : rest) -> mapper firstLetter : rest
    constructorPrefix = mapFirstLetter Char.toUpper prefix
    fieldPrefix = mapFirstLetter Char.toLower prefix

class HasJSONPrefix a where
  type JSONPrefix a :: Symbol

getPrefix :: Name -> Q String
getPrefix name = do
  hasInstance <- isInstance ''HasJSONPrefix [ConT name]
  if hasInstance
    then pure ()
    else fail $ "Missing HasJSONPrefix instance with a String prefix for " ++ show name

  info <- reify ''JSONPrefix
  let findPrefix :: [Dec] -> Maybe String
      findPrefix [] = Nothing
      findPrefix
        (TySynInstD (TySynEqn Nothing (AppT (ConT impl) (ConT name')) (LitT (StrTyLit prefix))) : _)
          | impl == ''JSONPrefix && name' == name =
              Just prefix
      findPrefix (_ : rest) = findPrefix rest
  case info of
    FamilyI _ decs ->
      case findPrefix decs of
        Nothing -> fail $ "Couldn't find String literal JSONPrefix for " ++ show name
        Just prefix -> pure prefix
    _ -> fail "JSONPrefix is not a type family"

deriveJSONPrefixed :: Name -> Q [Dec]
deriveJSONPrefixed name = do
  prefix <- getPrefix name
  deriveJSON (ourAesonOptionsPrefix prefix) name

deriveToJSONPrefixed :: Name -> Q [Dec]
deriveToJSONPrefixed name = do
  prefix <- getPrefix name
  deriveToJSON (ourAesonOptionsPrefix prefix) name

deriveFomJSONPrefixed :: Name -> Q [Dec]
deriveFomJSONPrefixed name = do
  prefix <- getPrefix name
  deriveFromJSON (ourAesonOptionsPrefix prefix) name
