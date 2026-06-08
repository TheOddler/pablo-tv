{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Actions qualified
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Directory.Directories qualified as Directories
import Directory.Files qualified as Files
import Directory.Paths qualified as Paths
import Elm.Derive (SumEncoding (..))
import Elm.Module (recAlterType)
import Elm.TyRep (EAlias (..), EPrimAlias (..), ESum (..), ETCon (..), ETypeDef (..), ETypeName (..), IsElmDefinition (..), SumEncoding' (..), SumTypeConstructor (..), SumTypeFields (..))
import ElmHelpers (deriveElmPrefixed, eTypeDefStringAlias, eTypeDict)
import Mpris qualified
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Method (Method)
import Servant
import Servant.Elm
import Servant.Elm.Internal.Foreign (LangElm)
import Servant.Foreign (GenerateList (..), HasForeign (..), Req)
import Server qualified
import System.IO (stdout)

-- | This seems to be needed for Raw endpoints, so provide a dummy implementation as we don't actually need to use them in Elm
instance GenerateList EType (Method -> Req EType) where
  generateList :: (Method -> Req EType) -> [Req EType]
  generateList _ = []

-- | This seems to be needed if I have any RawM endpoints. I essentially use the implementation for Raw
instance HasForeign LangElm ftype RawM where
  type Foreign ftype RawM = HTTP.Method -> Req ftype
  foreignFor lang ftype _api = foreignFor lang ftype (Proxy :: Proxy Raw)

deriveElmPrefixed ''Actions.MouseButton
deriveElmPrefixed ''Actions.KeyboardButton
deriveElmPrefixed ''Actions.Action
deriveElmPrefixed ''Mpris.MprisAction
deriveElmPrefixed ''Directories.RootDirectoryData
deriveElmPrefixed ''Directories.DirectoryData
deriveElmPrefixed ''Files.VideoFileData

instance IsElmDefinition Directories.RootDirectoryLocation where
  compileElmDef _ = eTypeDefStringAlias "RootDirectoryLocation"

instance IsElmDefinition Directories.DirectoryName where
  compileElmDef _ = eTypeDefStringAlias "DirectoryName"

instance IsElmDefinition Files.VideoFileName where
  compileElmDef _ = eTypeDefStringAlias "VideoFileName"

instance IsElmDefinition Files.ImageFileName where
  compileElmDef _ = eTypeDefStringAlias "ImageFileName"

instance IsElmDefinition Files.CachedImageFileName where
  compileElmDef _ = eTypeDefStringAlias "CachedImageFileName"

instance IsElmDefinition Paths.RawWebPath where
  compileElmDef _ = eTypeDefStringAlias "RawWebPath"

instance IsElmDefinition Directories.DirectorySubDirs where
  compileElmDef _ =
    ETypeSum $
      ESum
        { es_name = ETypeName "DirectorySubDirs" [],
          es_constructors =
            [ STC "DirectorySubDirs" "DirectorySubDirs" $
                Anonymous [eTypeDict "DirectoryName" "DirectoryData"]
            ],
          es_type = SumEncoding' UntaggedValue,
          es_omit_null = True,
          es_unary_strings = True
        }

instance IsElmDefinition Directories.RootDirectories where
  compileElmDef _ =
    ETypePrimAlias $
      EPrimAlias
        (ETypeName "RootDirectories" [])
        (eTypeDict "RootDirectoryLocation" "RootDirectoryData")

instance IsElmDefinition Files.Image where
  compileElmDef _ =
    ETypeAlias $
      EAlias
        { ea_name = ETypeName "Image" [],
          ea_fields =
            [ ("name", toElmType (Proxy :: Proxy (Maybe String))),
              ("cached", toElmType (Proxy :: Proxy String))
            ],
          ea_omit_null = True,
          ea_newtype = True,
          ea_unwrap_unary = False
        }

main :: IO ()
main = do
  BS.hPut stdout "Generating elm...\n"
  generateElmModuleWith
    ( defElmOptions
        { urlPrefix = Static "",
          elmAlterations = myAlterations,
          elmTypeAlterations = myTypeAlterations,
          elmToString = myElmToString
        }
    )
    [ "Generated",
      "Backend"
    ]
    elmImports
    "./frontend/src"
    myTypeDefs
    (Proxy :: Proxy (ToServantApi Server.APIRoutes))
  BS.hPut stdout "Done!\n"

myTypeDefs :: [DefineElm]
myTypeDefs =
  [ DefineElm (Proxy :: Proxy Actions.MouseButton),
    DefineElm (Proxy :: Proxy Actions.KeyboardButton),
    DefineElm (Proxy :: Proxy Actions.Action),
    DefineElm (Proxy :: Proxy Mpris.MprisAction),
    DefineElm (Proxy :: Proxy Directories.RootDirectories),
    DefineElm (Proxy :: Proxy Directories.RootDirectoryData),
    DefineElm (Proxy :: Proxy Directories.RootDirectoryLocation),
    DefineElm (Proxy :: Proxy Directories.DirectoryData),
    DefineElm (Proxy :: Proxy Directories.DirectorySubDirs),
    DefineElm (Proxy :: Proxy Directories.DirectoryName),
    DefineElm (Proxy :: Proxy Files.VideoFileName),
    DefineElm (Proxy :: Proxy Files.VideoFileData),
    DefineElm (Proxy :: Proxy Files.Image),
    DefineElm (Proxy :: Proxy Files.ImageFileName),
    DefineElm (Proxy :: Proxy Files.CachedImageFileName),
    DefineElm (Proxy :: Proxy Paths.RawWebPath)
  ]

myAlterations :: ETypeDef -> ETypeDef
myAlterations = \case
  other -> recAlterType myTypeAlterations other

myTypeAlterations :: EType -> EType
myTypeAlterations = \case
  ETyCon (ETCon "Int32") -> toElmType (Proxy :: Proxy Int)
  ETyCon (ETCon "Scientific") -> toElmType (Proxy :: Proxy Float)
  ETyCon (ETCon "NoContent") -> toElmType (Proxy :: Proxy ())
  ETyApp (ETyApp (ETyCon (ETCon "Map")) k) v
    | k `elem` stringAliases ->
        ETyApp
          ( ETyApp
              (ETyCon $ ETCon "Dict")
              k
          )
          v
  other -> defaultTypeAlterations other
  where
    stringAliases :: [EType]
    stringAliases =
      catMaybes
        [ case compileElmDef typeDef of
            ETypePrimAlias
              ( EPrimAlias
                  (ETypeName name [])
                  (ETyCon (ETCon "String"))
                ) ->
                Just $ ETyCon (ETCon name)
            _ -> Nothing
        | DefineElm typeDef <- myTypeDefs
        ]

myElmToString :: EType -> Text
myElmToString = defaultElmToString

elmImports :: Text
elmImports =
  T.unlines
    [ "-- The Json.Helpers module comes from bartavelle/json-helpers",
      "",
      "import Dict exposing (Dict)",
      "import Http",
      "import Iso8601",
      "import Json.Decode",
      "import Json.Encode exposing (Value)",
      "import Json.Helpers exposing (decodeSumTaggedObject, decodeSumUnaries, encodeMap, encodeObject, encodeSumTaggedObject, encodeValue, fnullable, maybeEncode, required)",
      "import Set",
      "import Time exposing (Posix)",
      "import Url.Builder",
      "",
      "",
      "jsonDecPosix : Json.Decode.Decoder Posix",
      "jsonDecPosix =",
      "    Iso8601.decoder",
      "",
      "",
      "jsonEncPosix : Posix -> Value",
      "jsonEncPosix =",
      "    Iso8601.encode",
      "",
      "",
      "{-| A hack where I replace Json.Helpers.decodeMap with my own as it errors and all my dicts have a String alias as Key anyway",
      "-}",
      "decodeMap : Json.Decode.Decoder String -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict String v)",
      "decodeMap _ decVal = Json.Decode.dict decVal"
    ]
