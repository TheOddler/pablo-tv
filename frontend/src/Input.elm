module Input exposing (..)

import ButtonsGrid as BG
import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Http
import Svg exposing (..)
import Svg.Attributes as SvgAttr


type alias Model =
    { fingerX : Float
    , fingerY : Float
    }


init : Model
init =
    { fingerX = 0
    , fingerY = 0
    }


type Msg
    = DoAction BE.Action
    | GotActionResult (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Http.Error )
update msg model =
    case msg of
        DoAction action ->
            ( model
            , BE.postApiAction action GotActionResult
            , Nothing
            )

        GotActionResult (Ok ()) ->
            ( model
            , -- Contrary to actions in the rest of the app, we do not want to request a data update here as we know the actions here are very unlikely to change the data.
              -- The only way really they might change the data is when you use the mouse to make changed on the tv interface, but then we'll get the update of the data soon enough through polling or websockets or something
              Cmd.none
            , Nothing
            )

        GotActionResult (Err err) ->
            ( model
            , Cmd.none
            , Just err
            )


view : Html Msg
view =
    let
        -- SVG converted using https://html-to-elm.com/
        leftMouseSvg =
            svg
                [ SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.width "32"
                , SvgAttr.height "32"
                , SvgAttr.color "#000000"
                , SvgAttr.fill "none"
                ]
                [ path
                    [ SvgAttr.d "M10 2.09084C10.6655 2.02383 11.3389 2 12 2C12.9247 2 13.8373 2.07762 14.7349 2.1882C17.1758 2.48893 19.0694 4.51965 19.2593 6.91118C19.3909 8.56832 19.5 10.268 19.5 12C19.5 13.732 19.3909 15.4317 19.2593 17.0888C19.0694 19.4803 17.1758 21.511 14.7349 21.8118C13.8373 21.9223 12.9247 22 12 22C11.0752 22 10.1626 21.9223 9.26502 21.8118C6.82417 21.511 4.93047 19.4803 4.74061 17.0888C4.60903 15.4317 4.5 13.732 4.5 12C4.5 11.3283 4.5164 10.6614 4.54415 10"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []
                , path
                    [ SvgAttr.d "M12 2L12 11"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    []
                , path
                    [ SvgAttr.d "M5 11H19"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    []
                , Svg.circle
                    [ SvgAttr.cx "5.5"
                    , SvgAttr.cy "5"
                    , SvgAttr.r "2.5"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    ]
                    []
                ]

        rightMouseSvg =
            svg
                [ SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.width "32"
                , SvgAttr.height "32"
                , SvgAttr.color "#000000"
                , SvgAttr.fill "none"
                ]
                [ path
                    [ SvgAttr.d "M13.5 2.09084C12.8345 2.02383 12.1611 2 11.5 2C10.5753 2 9.66267 2.07762 8.7651 2.1882C6.32417 2.48893 4.43061 4.51965 4.24069 6.91118C4.10906 8.56832 4 10.268 4 12C4 13.732 4.10906 15.4317 4.24069 17.0888C4.43061 19.4803 6.32417 21.511 8.7651 21.8118C9.66267 21.9223 10.5753 22 11.5 22C12.4248 22 13.3374 21.9223 14.235 21.8118C16.6758 21.511 18.5695 19.4803 18.7594 17.0888C18.891 15.4317 19 13.732 19 12C19 11.3283 18.9836 10.6614 18.9558 10"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []
                , path
                    [ SvgAttr.d "M11.5 2L11.5 11"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    []
                , path
                    [ SvgAttr.d "M18.5 11H4.5"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    []
                , Svg.circle
                    [ SvgAttr.cx "2.5"
                    , SvgAttr.cy "2.5"
                    , SvgAttr.r "2.5"
                    , SvgAttr.transform "matrix(-1 0 0 1 20 2.5)"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "1.5"
                    ]
                    []
                ]
    in
    BG.buttonsGrid [ A.id "input-container" ]
        [ BG.row
            [ div [ A.id "keyboard", A.class "button double-width" ]
                (BG.icon "fa-solid fa-keyboard")
            , div [ A.id "move-cursor-left", A.class "button" ]
                (BG.icon "fa-solid fa-chevron-left")
            , div [ A.id "move-cursor-right", A.class "button" ]
                (BG.icon "fa-solid fa-chevron-right")
            , div [ A.id "backspace", A.class "button" ]
                (BG.icon "fa-solid fa-delete-left")
            , div [ A.id "recenter", A.class "button" ]
                (BG.icon "fa-solid fa-arrows-to-dot")
            ]
        , div [ A.class "row grow" ]
            [ div [ A.id "trackpad" ] []
            ]
        , BG.row
            [ div [ A.id "left", A.class "button flat" ] [ leftMouseSvg ]
            , div [ A.id "pointer", A.class "button flat" ]
                (BG.icon "fa-solid fa-wand-magic-sparkles")
            , div [ A.id "right", A.class "button flat" ] [ rightMouseSvg ]
            ]
        ]
