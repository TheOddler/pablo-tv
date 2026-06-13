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
    | DirHome
    | Dir DirPath
    | Input
    | Remote
    | IPs
    | Debug_
    | NotFound


parse : List RootDirectoryLocation -> Url.Url -> Route
parse roots url =
    let
        path =
            Url.percentDecode url.path
                |> Maybe.withDefault url.path
                |> (\p ->
                        if String.endsWith "/" p then
                            String.dropRight 1 p

                        else
                            p
                   )

        try p result =
            if path == p then
                Just result

            else
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
    try "" Home
        |> Maybe.orElse (try "/dir" DirHome)
        |> Maybe.orElse (try "/input" Input)
        |> Maybe.orElse (try "/remote" Remote)
        |> Maybe.orElse (try "/ips" IPs)
        |> Maybe.orElse (try "/debug" Debug_)
        |> Maybe.orElse (try "/404" NotFound)
        |> Maybe.orElse (List.findMap tryRoot roots)
        |> Maybe.withDefault NotFound


toHref : Route -> String
toHref route =
    case route of
        Home ->
            "/"

        DirHome ->
            "/dir"

        Dir (DirPath root paths) ->
            "/dir/" ++ root ++ "/" ++ String.join "/" paths

        Input ->
            "/input"

        Remote ->
            "/remote"

        IPs ->
            "/ips"

        Debug_ ->
            "/debug"

        NotFound ->
            "/404"
