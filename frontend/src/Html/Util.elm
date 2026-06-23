module Html.Util exposing (..)

import Html exposing (..)


{-| A non-breaking space. elm-html doesn't support escape sequences like `text "&nbsp"`, so use this instead
-}
nbsp : Html msg
nbsp =
    text "\u{00A0}"
