module Main exposing (Msg(..), main, update, view)

import Browser
import Dict
import Generated.Backend exposing (..)
import Home
import Html exposing (..)
import Html.Attributes as A
import Http
import Random


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
        aggInfos =
            Home.calcAggInfos model.data

        seed1 =
            Random.initialSeed model.startTime

        seed2 =
            Random.initialSeed <| model.startTime + 42

        viewRow filter sorting =
            Home.viewRow <|
                Home.filterAndSort filter sorting aggInfos
    in
    div [ A.id "home-container" ]
        [ h1 [] [ text "Watching" ]
        , viewRow Home.Watching Home.RecentlyWatched
        , h1 [] [ text "New" ]
        , viewRow Home.NothingWatched Home.RecentlyAdded
        , h1 [] [ text "Random" ]
        , viewRow Home.NothingWatched (Home.Shuffled seed1)
        , h1 [] [ text "Recently Added" ]
        , viewRow Home.Unfiltered Home.RecentlyAdded
        , h1 [] [ text "Random (All)" ]
        , viewRow Home.Unfiltered (Home.Shuffled seed2)
        , h1 [] [ text "Recently Finished" ]
        , viewRow Home.FullyWatched Home.RecentlyWatched
        ]
