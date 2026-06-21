module QR exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import QRCode
import Svg.Attributes as SvgA


type Error
    = NoTopInterfaceFound
    | ErrorGeneratingQRCode QRCode.Error


urlQrCode_ : List (Attribute msg) -> BE.NetworkInfo -> Result Error (Html msg)
urlQrCode_ attrs info =
    case List.head info.interfaces of
        Nothing ->
            Err NoTopInterfaceFound

        Just interface ->
            let
                url =
                    "http://" ++ interface.ipv4 ++ ":" ++ String.fromInt info.port_
            in
            case QRCode.fromStringWith QRCode.Low url of
                Ok qr ->
                    Ok <|
                        QRCode.toSvgWithoutQuietZone
                            (SvgA.class "qr" :: attrs)
                            qr

                Err e ->
                    Err <| ErrorGeneratingQRCode e


{-| We know urlQrCode\_ should be safe for any given network info in our system, so this just ignores any error we likely won't get anyway.
-}
urlQrCode : List (Attribute msg) -> BE.NetworkInfo -> Html msg
urlQrCode attrs info =
    case urlQrCode_ attrs info of
        Ok html ->
            html

        Err _ ->
            text ""


errorToString : QRCode.Error -> String
errorToString e =
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
