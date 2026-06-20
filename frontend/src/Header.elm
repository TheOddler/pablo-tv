module Header exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import QRCode
import Routes
import Svg.Attributes as SvgA


view : Routes.Route -> BE.NetworkInfo -> (Int -> msg) -> (BE.Action -> msg) -> Html msg
view route networkInfo navBack doAction =
    let
        separator =
            div [ A.class "separator" ] []

        back =
            [ button
                [ A.class "like-link", E.onClick <| navBack 1 ]
                [ i [ A.class "fa-solid fa-chevron-left" ] [] ]
            , separator
            ]

        qrCode =
            case List.head networkInfo.interfaces of
                Nothing ->
                    []

                Just interface ->
                    let
                        url =
                            "http://"
                                ++ interface.ipv4
                                ++ ":"
                                ++ String.fromInt networkInfo.port_
                    in
                    [ mkQrCode url
                    , separator
                    ]

        mkQrCode : String -> Html msg
        mkQrCode message =
            case QRCode.fromStringWith QRCode.Low message of
                Ok qr ->
                    QRCode.toSvgWithoutQuietZone
                        [ SvgA.class "url-qr" ]
                        qr

                Err e ->
                    Html.text <|
                        case e of
                            QRCode.AlignmentPatternNotFound ->
                                "AlignmentPatternNotFound"

                            QRCode.InvalidNumericChar ->
                                "InvalidNumericChar"

                            QRCode.InvalidAlphanumericChar ->
                                "InvalidAlphanumericChar"

                            QRCode.InvalidUTF8Char ->
                                "InvalidUTF8Char"

                            QRCode.LogTableException i ->
                                "LogTableException " ++ String.fromInt i

                            QRCode.PolynomialMultiplyException ->
                                "PolynomialMultiplyException"

                            QRCode.PolynomialModException ->
                                "PolynomialModException"

                            QRCode.InputLengthOverflow ->
                                "InputLengthOverflow"

        networkInterface =
            case List.head networkInfo.interfaces of
                Nothing ->
                    []

                Just interface ->
                    let
                        url =
                            interface.ipv4 ++ ":" ++ String.fromInt networkInfo.port_
                    in
                    [ a
                        [ A.class "shrinking"
                        , A.href <| Routes.toHref Routes.IPs
                        ]
                        [ i [ A.class "fa-solid fa-mobile-screen-button" ] []
                        , text url
                        ]
                    , separator
                    ]

        refreshButton : String -> BE.Action -> Html msg
        refreshButton suffix action =
            button
                [ A.class "like-link shrinking", E.onClick <| doAction action ]
                [ i [ A.class "fa-solid fa-arrows-rotate" ] []
                , span [] [ text "Refresh" ]
                , span [] [ text suffix ]
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
            ++ qrCode
            ++ networkInterface
            ++ dirRefresh
            ++ otherButtons
