module Routes exposing (..)

-- I don't use Url.Parser from elm/url as it doesn't seem to support lists nor recursion

import Generated.Backend exposing (DirectoryName, RootDirectoryLocation)
import List.Extra as List
import Maybe.Extra as Maybe
import Url


type DirPath
    = DirPath RootDirectoryLocation (List DirectoryName)


type Route
    = Home
    | Dir DirPath


parse : List RootDirectoryLocation -> Url.Url -> Route
parse roots url =
    let
        path =
            url.path

        tryHome =
            case path of
                "" ->
                    Just Home

                "/" ->
                    Just Home

                _ ->
                    Nothing

        tryRoot root =
            let
                prefix =
                    "/dir/" ++ root ++ "/"
            in
            if String.startsWith prefix path then
                String.dropLeft (String.length prefix) path
                    |> String.split "/"
                    |> DirPath root
                    |> Dir
                    |> Just

            else
                Nothing
    in
    tryHome
        |> Maybe.orElse (List.findMap tryRoot roots)
        |> Maybe.withDefault Home


toHref : Route -> String
toHref route =
    case route of
        Home ->
            "/"

        Dir (DirPath root paths) ->
            "/dir/" ++ root ++ "/" ++ String.join "/" paths
