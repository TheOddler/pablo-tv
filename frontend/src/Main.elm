module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document)
import Dict
import Generated.Backend exposing (RootDirectories, getApiData)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Value, value)
import Url.Builder


type alias Model =
    { data : RootDirectories
    , errors : List Http.Error
    }


type Msg
    = DirsUpdate (Result Http.Error RootDirectories)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = Dict.empty, errors = [] }
    , getApiData DirsUpdate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DirsUpdate (Ok updated) ->
            ( { model | data = updated }
            , Cmd.none
            )

        DirsUpdate (Err err) ->
            registerError err model


registerError : Http.Error -> Model -> ( Model, Cmd Msg )
registerError err model =
    ( { model | errors = err :: model.errors }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Pablo TV"
    , body =
        [ text <| Debug.toString model.data
        , div [] <|
            List.map (\err -> text <| Debug.toString err) model.errors
        ]
    }
