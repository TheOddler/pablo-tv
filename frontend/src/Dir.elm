module Dir exposing (..)

import Browser
import Generated.Backend exposing (RootDirectories)
import Html exposing (a, text)
import Html.Attributes as A
import Routes


view :
    RootDirectories
    -> Routes.DirPath
    -> Browser.Document msg
view roots path =
    let
        (Routes.DirPath rootLoc pathParts) =
            path
    in
    { title = String.join "/" pathParts ++ " - Pablo TV"
    , body =
        [ text <| Routes.toHref (Routes.Dir path) ++ " - Pablo TV"
        , a
            [ A.href <| Routes.toHref Routes.Home
            ]
            [ text "Back home" ]
        ]
    }
