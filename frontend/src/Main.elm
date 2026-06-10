module Main exposing (Msg(..), main, update, view)

import Browser
import Dict
import Generated.Backend exposing (..)
import Home
import Html exposing (..)
import Html.Attributes as A
import Http
import Random
import Random.List


type alias Model =
    { startTime : Int
    , data : RootDirectories
    , errors : List Http.Error
    }


type alias Flags =
    { startTime : Int
    }


type Msg
    = DirsUpdate (Result Http.Error RootDirectories)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { startTime = flags.startTime
      , data = Dict.empty
      , errors = []
      }
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


view : Model -> Browser.Document Msg
view model =
    { title = "Pablo TV"
    , body =
        [ text <| String.join ", " <| Dict.keys model.data
        , viewHome model
        ]
    }


viewHome : Model -> Html Msg
viewHome model =
    let
        shuffle : Random.Seed -> List a -> List a
        shuffle seed list =
            Random.step (Random.List.shuffle list) seed
                |> Tuple.first

        aggInfos =
            Home.calcAggInfos model.data
    in
    div [ A.id "home-container" ]
        [ h1 [] [ text "Watching" ]
        , Home.viewRow aggInfos
        , h1 [] [ text "New" ]
        , Home.viewRow aggInfos
        , h1 [] [ text "Random" ]
        , Home.viewRow <| shuffle (Random.initialSeed model.startTime) aggInfos
        ]
