module Header exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import QR
import Routes
import Svg.Attributes as SvgA


view : Routes.Route -> BE.NetworkInfo -> Bool -> (Int -> msg) -> (BE.Action -> msg) -> Html msg
view route networkInfo isRefreshing navBack doAction =
    let
        separator =
            div [ A.class "separator" ] []

        back =
            [ button
                [ A.class "like-link", E.onClick <| navBack 1 ]
                [ i [ A.class "fa-solid fa-chevron-left" ] [] ]
            , separator
            ]

        networkInterface =
            case List.head networkInfo.interfaces of
                Nothing ->
                    []

                Just interface ->
                    let
                        baseUrl =
                            interface.ipv4 ++ ":" ++ String.fromInt networkInfo.port_
                    in
                    [ a
                        [ A.href <| Routes.toHref Routes.IPs
                        ]
                        [ QR.urlQrCode [ SvgA.class "url-qr" ] networkInfo
                        , span [ A.class "auto-hiding-label hides-second" ]
                            [ text baseUrl ]
                        ]
                    , separator
                    ]

        refreshButton : String -> BE.Action -> Html msg
        refreshButton dirDescr action =
            button
                [ A.class "like-link"
                , A.disabled isRefreshing
                , A.title <| "Refresh " ++ dirDescr
                , E.onClick <| doAction action
                ]
                [ i
                    [ A.class "fa-solid fa-arrows-rotate"
                    , A.class <|
                        if isRefreshing then
                            "rotating"

                        else
                            ""
                    ]
                    []
                , span [ A.class "auto-hiding-label hides-first" ]
                    [ text "Refresh" ]
                ]

        dirRefresh =
            case route of
                Routes.Home ->
                    [ refreshButton "library" BE.ActionRefreshAllDirectoryData
                    , separator
                    ]

                Routes.DirHome ->
                    [ refreshButton "library" BE.ActionRefreshAllDirectoryData
                    , separator
                    ]

                Routes.Dir dirPath ->
                    [ refreshButton "directory" <|
                        BE.ActionRefreshDirectoryData
                            { path = Routes.toRawWebPath dirPath }
                    , separator
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
