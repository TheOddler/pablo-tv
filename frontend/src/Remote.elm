module Remote exposing (..)

import ButtonsGrid as BG
import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A


view : (BE.Action -> msg) -> Html msg
view doAction =
    let
        button =
            BG.button doAction

        buttonRed =
            BG.buttonRed doAction

        buttonLarge =
            BG.buttonLarge doAction

        buttonDouble =
            BG.buttonDouble doAction
    in
    BG.buttonsGrid []
        [ BG.row
            [ buttonRed (BG.mpris BE.MprisQuit) "fa-solid fa-power-off"
            , BG.emptySpace
            , BG.emptySpace
            , button (BG.mpris BE.MprisGoWindowed) "fa-solid fa-compress"
            , button (BG.mpris BE.MprisGoFullscreen) "fa-solid fa-expand"
            ]
        , BG.row
            [ BG.emptySpace
            , button (BG.mpris BE.MprisPrevious) "fa-solid fa-angles-left"
            , div [ A.class "large" ] [ text "Play", wbr [] [], text "list" ]
            , button (BG.mpris BE.MprisNext) "fa-solid fa-angles-right"
            , BG.emptySpace
            ]
        , BG.spacer
        , BG.row
            [ BG.emptySpace
            , buttonLarge
                (BG.mpris BE.MprisPlayPause)
                "fa-solid fa-play"
                "fa-solid fa-pause"
            , buttonDouble
                (BG.keyboard BE.KeyboardVolumeUp)
                "fa-solid fa-plus"
                "fa-solid fa-volume-high"
                (BG.keyboard BE.KeyboardVolumeDown)
                "fa-solid fa-minus"
            ]
        , BG.row
            [ button (BG.mpris BE.MprisBackwardJump) "fa-solid fa-backward-fast"
            , button (BG.mpris BE.MprisBackwardStep) "fa-solid fa-backward-step"
            , button (BG.mpris BE.MprisForwardStep) "fa-solid fa-forward-step"
            , button (BG.mpris BE.MprisForwardJump) "fa-solid fa-forward-fast"
            ]
        ]
