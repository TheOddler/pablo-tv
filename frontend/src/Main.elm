module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict
import Dir
import Error
import Generated.Backend exposing (..)
import Home
import Html exposing (..)
import Http
import Json.Decode as D
import Ports
import Routes
import Url


type alias Model =
    { startTime : Int
    , roots : RootDirectories
    , navKey : Nav.Key
    , route : Routes.Route
    , errors : List String
    }


type alias Flags =
    { startTime : Int
    , roots : Maybe String
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
        ( roots, errors ) =
            case flags.roots of
                Nothing ->
                    ( Dict.empty, [] )

                Just actualRoots ->
                    case D.decodeString Generated.Backend.jsonDecRootDirectories actualRoots of
                        Ok parsedRoots ->
                            ( parsedRoots, [] )

                        Err err ->
                            ( Dict.empty, [ D.errorToString err ] )
    in
    ( { startTime = flags.startTime
      , roots = roots
      , navKey = navKey
      , route = Routes.parse (Dict.keys roots) url
      , errors = errors
      }
    , getApiData DirsUpdate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DirsUpdate (Ok updated) ->
            ( { model | roots = updated }
            , Ports.saveRoots updated
            )

        DirsUpdate (Err err) ->
            registerHttpError err model

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


registerHttpError : Http.Error -> Model -> ( Model, Cmd Msg )
registerHttpError err model =
    ( { model | errors = Error.httpErrorToString err :: model.errors }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    let
        doc =
            case model.route of
                Routes.Home ->
                    Home.view model.roots model.startTime

                Routes.Dir path ->
                    Dir.view model.roots path
    in
    { title = doc.title
    , body =
        List.map (\e -> div [] [ text e ]) model.errors
            ++ doc.body
    }
