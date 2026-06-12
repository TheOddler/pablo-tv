{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Actions qualified
import Control.Applicative (asum)
import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Directory.Directories qualified as Directories
import Directory.Files qualified as Files
import Directory.Paths qualified as Paths
import Elm.Derive (SumEncoding (..))
import Elm.Module (recAlterType)
import Elm.TyRep (EAlias (..), EPrimAlias (..), ESum (..), ETCon (..), ETypeDef (..), ETypeName (..), IsElmDefinition (..), SumEncoding' (..), SumTypeConstructor (..), SumTypeFields (..))
import ElmHelpers (deriveElmPrefixed, eTypeDefStringAlias, eTypeDict)
import GHC.Exception (errorCallException)
import Mpris qualified
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Method (Method)
import Servant
import Servant.Elm
import Servant.Elm.Internal.Foreign (LangElm)
import Servant.Foreign (GenerateList (..), HasForeign (..), Req)
import Server qualified
import System.Environment (getArgs)
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

-- In Haskell Directories.DirectoryData is a recursive type, as it has sub-dirs of the same type.
-- However, in Elm Directories.DirectoryData is a type alias, and those cannot be recursive.
-- So we need a bit of a hack to allow for it, the "Less obvious, but nicer" here: https://github.com/elm/compiler/blob/master/hints/recursive-alias.md#less-obvious-but-nicer
data DirectorySubDirs

instance IsElmDefinition DirectorySubDirs where
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

recursiveDirectoryDataAlterations :: EType -> Maybe EType
recursiveDirectoryDataAlterations = \case
  -- The Haskell type is a `Map DirectoryName DirectoryData`, so we replace that with `DirectorySubDirs` in Elm
  ETyApp
    ( ETyApp
        (ETyCon (ETCon "Map"))
        (ETyCon (ETCon "DirectoryName"))
      )
    (ETyCon (ETCon "DirectoryData")) ->
      Just (ETyCon (ETCon "DirectorySubDirs"))
  _ -> Nothing

-- | Some helpers to have easier access to the subDirs Dict
recursiveDirectoryDataHelpers :: Text
recursiveDirectoryDataHelpers =
  T.unlines
    [ "unDirectorySubDirs : DirectorySubDirs -> Dict DirectoryName DirectoryData",
      "unDirectorySubDirs (DirectorySubDirs inner) = inner",
      "",
      "",
      "subDirsUnwrapped : {a | subDirs : DirectorySubDirs} -> Dict DirectoryName DirectoryData",
      "subDirsUnwrapped data = unDirectorySubDirs data.subDirs"
    ]

main :: IO ()
main = do
  args <- getArgs
  let staticArg = "--out="
  outDir <- case asum (stripPrefix staticArg <$> args) of
    Nothing -> throwIO . errorCallException $ "Missing " ++ staticArg ++ " argument"
    Just "" -> throwIO . errorCallException $ "Empty " ++ staticArg ++ " argument"
    Just path -> pure path

  BS.hPut stdout "Generating elm...\n"
  generateElmModuleWith
    ( defElmOptions
        { urlPrefix = Static "",
          elmAlterations = recAlterType allTypeAlterations,
          elmTypeAlterations = allTypeAlterations,
          elmToString = myElmToString
        }
    )
    [ "Generated",
      "Backend"
    ]
    elmImports
    outDir
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
    DefineElm (Proxy :: Proxy DirectorySubDirs),
    DefineElm (Proxy :: Proxy Directories.DirectoryName),
    DefineElm (Proxy :: Proxy Files.VideoFileName),
    DefineElm (Proxy :: Proxy Files.VideoFileData),
    DefineElm (Proxy :: Proxy Files.Image),
    DefineElm (Proxy :: Proxy Files.ImageFileName),
    DefineElm (Proxy :: Proxy Files.CachedImageFileName),
    DefineElm (Proxy :: Proxy Paths.RawWebPath)
  ]

allTypeAlterations :: EType -> EType
allTypeAlterations et =
  fromMaybe (defaultTypeAlterations et) $
    asum
      [ recursiveDirectoryDataAlterations et,
        myTypeAlterations et
      ]

myTypeAlterations :: EType -> Maybe EType
myTypeAlterations = \case
  ETyCon (ETCon "Int32") -> Just $ toElmType (Proxy :: Proxy Int)
  ETyCon (ETCon "Scientific") -> Just $ toElmType (Proxy :: Proxy Float)
  ETyCon (ETCon "NoContent") -> Just $ toElmType (Proxy :: Proxy ())
  ETyApp (ETyApp (ETyCon (ETCon "Map")) k) v
    | k `elem` stringAliases ->
        Just $
          ETyApp
            ( ETyApp
                (ETyCon $ ETCon "Dict")
                k
            )
            v
  _ -> Nothing
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
      "import Json.Helpers exposing (decodeSumTaggedObject, decodeSumUnaries, encodeObject, encodeSumTaggedObject, encodeValue, fnullable, maybeEncode, required)",
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
      "decodeMap _ decVal = Json.Decode.dict decVal",
      "",
      "",
      "{-| A hack where I replace Json.Helpers.encodeMap with my own as it encodes the string keys of the dict with extra quotes around it",
      "-}",
      "encodeMap : (String -> Value) -> (v -> Value) -> Dict String v -> Value",
      "encodeMap _ encVal =",
      "    Json.Encode.dict identity encVal",
      "",
      "",
      recursiveDirectoryDataHelpers
    ]
