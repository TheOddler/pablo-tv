module Error exposing (..)

import Http exposing (Error(..))


viewHttpError : Http.Error -> String
viewHttpError err =
    case err of
        Http.BadUrl msg ->
            "BadUrl " ++ msg

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus code ->
            "BadStatus " ++ String.fromInt code

        Http.BadBody msg ->
            "BadBody " ++ msg
