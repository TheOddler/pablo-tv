module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict
import Dir
import Generated.Backend exposing (..)
import Home
import Html exposing (..)
import Http
import Routes
import Url


type alias Model =
    { startTime : Int
    , roots : RootDirectories
    , navKey : Nav.Key
    , route : Routes.Route
    , errors : List Http.Error
    }


type alias Flags =
    { startTime : Int
    }


type Msg
    = DirsUpdate (Result Http.Error RootDirectories)
    | GotUrlRequest Browser.UrlRequest
    | UrlChanged Url.Url


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = GotUrlRequest
        , onUrlChange = UrlChanged
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        roots =
            -- TODO: Save the roots to the local storage, send through flags, and then parse here
            Dict.empty
    in
    ( { startTime = flags.startTime
      , roots = roots
      , navKey = navKey
      , route = Routes.parse (Dict.keys roots) url
      , errors = []
      }
    , getApiData DirsUpdate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DirsUpdate (Ok updated) ->
            ( { model | roots = updated }
            , Cmd.none
            )

        DirsUpdate (Err err) ->
            registerError err model

        GotUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Routes.parse (Dict.keys model.roots) url }
            , Cmd.none
            )


registerError : Http.Error -> Model -> ( Model, Cmd Msg )
registerError err model =
    ( { model | errors = err :: model.errors }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Routes.Home ->
            Home.view model.roots model.startTime

        Routes.Dir path ->
            Dir.view model.roots path
