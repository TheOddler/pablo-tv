module ButtonsGrid exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E


buttonsGrid : List (Attribute msg) -> List (Html msg) -> Html msg
buttonsGrid extraAttrs =
    div (A.class "buttons-grid" :: extraAttrs)


row : List (Html msg) -> Html msg
row =
    div [ A.class "row" ]


spacer : Html msg
spacer =
    div [ A.class "spacer" ] []


button_ : (BE.Action -> msg) -> List (Attribute msg) -> BE.Action -> List (Html msg) -> Html msg
button_ doAction extraAttrs action body =
    Html.button
        (E.onClick (doAction action) :: extraAttrs)
        body


button : (BE.Action -> msg) -> BE.Action -> String -> Html msg
button doAction action iconClass =
    button_ doAction [] action [ i [ A.class iconClass ] [] ]


buttonRed : (BE.Action -> msg) -> BE.Action -> String -> Html msg
buttonRed doAction action iconClass =
    button_ doAction [ A.class "red" ] action [ i [ A.class iconClass ] [] ]


buttonLarge : (BE.Action -> msg) -> BE.Action -> String -> String -> Html msg
buttonLarge doAction action firstIcon secondIcon =
    button_ doAction
        [ A.class "large" ]
        action
        [ -- The extra div makes the scaling look better when button is pressed
          div []
            [ i [ A.class firstIcon ] []
            , i [ A.class secondIcon ] []
            ]
        ]


buttonDouble : (BE.Action -> msg) -> BE.Action -> String -> String -> BE.Action -> String -> Html msg
buttonDouble doAction actionTop iconTop middleIcon actionBottom iconBottom =
    div [ A.class "double-button" ]
        [ button doAction actionTop iconTop
        , i [ A.class middleIcon ] []
        , button doAction actionBottom iconBottom
        ]


mpris : BE.MprisAction -> BE.Action
mpris a =
    BE.ActionMedia { media = a }


keyboard : BE.KeyboardButton -> BE.Action
keyboard k =
    BE.ActionPressKeyboard { key = k }


icon : String -> List (Html msg)
icon iconClass =
    [ i [ A.class iconClass ] [] ]


emptySpace : Html msg
emptySpace =
    div [] []
