module Remote exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E


view : (BE.Action -> msg) -> Html msg
view doAction =
    let
        row =
            div [ A.class "row" ]

        spacer =
            div [ A.class "spacer" ] []

        button extraAttrs action body =
            Html.button
                (E.onClick (doAction action) :: extraAttrs)
                body

        mpris a =
            BE.ActionMedia { media = a }

        keyboard k =
            BE.ActionPressKeyboard { key = k }

        icon iconClass =
            [ i [ A.class iconClass ] [] ]

        emptySpace =
            div [] []
    in
    div [ A.id "buttons-grid" ]
        [ row
            [ button [ A.class "red" ] (mpris BE.MprisQuit) (icon "fa-solid fa-power-off")
            , emptySpace
            , emptySpace
            , button [] (mpris BE.MprisGoWindowed) (icon "fa-solid fa-compress")
            , button [] (mpris BE.MprisGoFullscreen) (icon "fa-solid fa-expand")
            ]
        , row
            [ emptySpace
            , button [] (mpris BE.MprisPrevious) (icon "fa-solid fa-angles-left")
            , div [ A.class "large" ] [ text "Play", wbr [] [], text "list" ]
            , button [] (mpris BE.MprisNext) (icon "fa-solid fa-angles-right")
            , emptySpace
            ]
        , spacer
        , row
            [ emptySpace
            , button
                [ A.class "large" ]
                (mpris BE.MprisPlayPause)
                [ --       $# The extra div makes the scaling look better when button is pressed
                  div []
                    [ i [ A.class "fa-solid fa-play" ] []
                    , i [ A.class "fa-solid fa-pause" ] []
                    ]
                ]
            , div [ A.class "double-button" ]
                [ button [] (keyboard BE.KeyboardVolumeUp) (icon "fa-solid fa-plus")
                , i [ A.class "fa-solid fa-volume-high" ] []
                , button [] (keyboard BE.KeyboardVolumeDown) (icon "fa-solid fa-minus")
                ]
            ]
        , row
            [ button [] (mpris BE.MprisBackwardJump) (icon "fa-solid fa-backward-fast")
            , button [] (mpris BE.MprisBackwardStep) (icon "fa-solid fa-backward-step")
            , button [] (mpris BE.MprisForwardStep) (icon "fa-solid fa-forward-step")
            , button [] (mpris BE.MprisForwardJump) (icon "fa-solid fa-forward-fast")
            ]
        ]
