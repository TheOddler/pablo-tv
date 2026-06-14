module Header exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Routes


view : Routes.Route -> BE.NetworkInfo -> (Int -> msg) -> (BE.Action -> msg) -> Html msg
view route networkInfo navBack doAction =
    let
        separatorGrow =
            div [ A.class "grow" ] []

        back =
            [ button
                [ A.class "like-link", E.onClick <| navBack 1 ]
                [ i [ A.class "fa-solid fa-chevron-left" ] [] ]
            , separatorGrow
            ]

        networkInterface =
            case List.head networkInfo.interfaces of
                Nothing ->
                    []

                Just interface ->
                    [ a [ A.href <| Routes.toHref Routes.IPs ]
                        [ i [ A.class "fa-solid fa-mobile-screen-button" ] []
                        , text <| interface.ipv4 ++ ":" ++ String.fromInt networkInfo.port_
                        ]
                    , separatorGrow
                    ]

        refreshButton : String -> BE.Action -> Html msg
        refreshButton suffix action =
            button
                [ A.class "like-link", E.onClick <| doAction action ]
                [ i [ A.class "fa-solid fa-arrows-rotate" ] []
                , span [] [ text "Refresh" ]
                , span [] [ text suffix ]
                ]

        dirRefresh =
            case route of
                Routes.Home ->
                    [ refreshButton "library" BE.ActionRefreshAllDirectoryData
                    , separatorGrow
                    ]

                Routes.DirHome ->
                    [ refreshButton "library" BE.ActionRefreshAllDirectoryData
                    , separatorGrow
                    ]

                Routes.Dir dirPath ->
                    [ refreshButton "directory" <|
                        BE.ActionRefreshDirectoryData
                            { path = Routes.toRawWebPath dirPath }
                    , separatorGrow
                    ]

                Routes.Input ->
                    []

                Routes.Remote ->
                    []

                Routes.IPs ->
                    []

                Routes.Debug_ ->
                    []

                Routes.NotFound ->
                    []

        otherButtons =
            [ a [ A.href <| Routes.toHref Routes.Input ]
                [ i [ A.class "fa-solid fa-wand-magic-sparkles" ] [] ]
            , a [ A.href <| Routes.toHref Routes.Remote ]
                [ i [ A.class "fa-solid fa-gamepad" ] [] ]
            , a [ A.href <| Routes.toHref <| Routes.DirHome ]
                [ i [ A.class "fa-solid fa-file-video" ] [] ]
            , a [ A.href <| Routes.toHref Routes.Home ]
                [ i [ A.class "fa-solid fa-house" ] [] ]
            ]
    in
    header [] <|
        back
            ++ networkInterface
            ++ dirRefresh
            ++ otherButtons
