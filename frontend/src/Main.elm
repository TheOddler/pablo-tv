module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict
import Dir
import Error
import Generated.Backend as BE
import Header
import Home
import Html exposing (..)
import Html.Attributes as A
import Http
import IPs
import Input
import Json.Decode as D
import LocalStorage
import Platform.Cmd as Cmd
import Remote
import Routes
import Task
import Time
import Url


type alias Model =
    { startTime : Int
    , roots : BE.RootDirectories
    , refreshing : Bool
    , networkInfo : BE.NetworkInfo
    , navKey : Nav.Key
    , route : Routes.Route
    , inputModel : Input.Model
    , errors : List String
    }


type alias Flags =
    { startTime : Int
    , roots : Maybe String
    , networkInfo : Maybe String
    }


type Msg
    = NoOp
    | GetDirsUpdate
    | GotDirsUpdateResult (Result Http.Error BE.RootDirectories)
    | NetworkInfoUpdate (Result Http.Error BE.NetworkInfo)
    | GotUrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | NavBack Int
    | DoAction BE.Action
    | GotActionResult BE.Action (Result Http.Error ())
    | InputMsg Input.Msg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = GotUrlRequest
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fiveSeconds =
            5000

        refreshData _ =
            GetDirsUpdate
    in
    -- Polling for now, we should replace this with websockets
    Time.every fiveSeconds refreshData


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        decodeFlag : Maybe String -> D.Decoder a -> ( Maybe a, List String )
        decodeFlag mFlagValue decoder =
            case mFlagValue of
                Nothing ->
                    ( Nothing, [] )

                Just flagValue ->
                    case D.decodeString decoder flagValue of
                        Ok parsedRoots ->
                            ( Just parsedRoots, [] )

                        Err err ->
                            ( Nothing, [ D.errorToString err ] )

        withFallback : a -> ( Maybe a, List String ) -> ( a, List String )
        withFallback fallback =
            Tuple.mapFirst (Maybe.withDefault fallback)

        ( roots, rootsErrors ) =
            decodeFlag flags.roots BE.jsonDecRootDirectories
                |> withFallback Dict.empty

        ( networkInfo, networkInfoErrors ) =
            decodeFlag flags.networkInfo BE.jsonDecNetworkInfo
                |> withFallback
                    { port_ = 8080
                    , interfaces = []
                    }
    in
    ( { startTime = flags.startTime
      , roots = roots
      , refreshing = False
      , networkInfo = networkInfo -- Network info is sorted when I save it to the local storage, so no need to sort again here
      , navKey = navKey
      , route = Routes.parse (Dict.keys roots) url
      , inputModel = Input.init
      , errors = rootsErrors ++ networkInfoErrors
      }
    , Cmd.batch
        [ BE.getApiData GotDirsUpdateResult
        , BE.getApiNetwork NetworkInfoUpdate
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        onHttpSuccess : Result Http.Error a -> (a -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
        onHttpSuccess result onSucc =
            case result of
                Ok ok ->
                    onSucc ok

                Err err ->
                    ( registerHttpError err model
                    , Cmd.none
                    )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetDirsUpdate ->
            ( model
            , BE.getApiData GotDirsUpdateResult
            )

        GotDirsUpdateResult res ->
            onHttpSuccess res <|
                \updated ->
                    ( -- This check makes sure calls to lazy work properly, otherwise even when there's no change
                      if model.roots /= updated then
                        { model | roots = updated }

                      else
                        model
                    , LocalStorage.saveRoots updated
                    )

        NetworkInfoUpdate res ->
            onHttpSuccess res <|
                \networkInfo ->
                    let
                        -- Sort these once, and then everywhere else in the code we assume they are sorted
                        sorted =
                            sortNetworkInfo networkInfo
                    in
                    ( { model | networkInfo = sortNetworkInfo networkInfo }
                    , LocalStorage.saveNetworkInfo sorted
                    )

        GotUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    Routes.parse (Dict.keys model.roots) url
            in
            if newRoute /= model.route then
                ( { model | route = newRoute }
                , Task.perform (\() -> NoOp) (Dom.setViewport 0 0)
                )

            else
                ( model, Cmd.none )

        NavBack steps ->
            ( model
            , Nav.back model.navKey steps
            )

        DoAction action ->
            case action of
                BE.ActionRefreshAllDirectoryData ->
                    ( { model | refreshing = True }
                    , BE.postApiAction action <| GotActionResult action
                    )

                BE.ActionRefreshDirectoryData _ ->
                    ( { model | refreshing = True }
                    , BE.postApiAction action <| GotActionResult action
                    )

                _ ->
                    ( model
                    , BE.postApiAction action <| GotActionResult action
                    )

        GotActionResult action result ->
            let
                ( updatedModel, cmd ) =
                    onHttpSuccess result <|
                        \() ->
                            ( model
                            , BE.getApiData GotDirsUpdateResult
                            )
            in
            case action of
                BE.ActionRefreshAllDirectoryData ->
                    ( { updatedModel | refreshing = False }
                    , cmd
                    )

                BE.ActionRefreshDirectoryData _ ->
                    ( { updatedModel | refreshing = False }
                    , cmd
                    )

                _ ->
                    ( updatedModel, cmd )

        InputMsg inputMsg ->
            let
                ( newInputModel, cmd, mError ) =
                    Input.update inputMsg model.inputModel

                newModel =
                    { model | inputModel = newInputModel }
            in
            ( case mError of
                Nothing ->
                    newModel

                Just err ->
                    registerHttpError err newModel
            , Cmd.map InputMsg cmd
            )


sortNetworkInfo : BE.NetworkInfo -> BE.NetworkInfo
sortNetworkInfo info =
    let
        networkInterfaceWorthiness : BE.SimpleNetworkInterface -> Int
        networkInterfaceWorthiness { name, ipv4 } =
            List.sum
                [ hasIpv4 ipv4 |> countAs -2
                , goodName name |> countAs -1
                , isLocalHost ipv4 |> countAs 10
                ]

        countAs val bool =
            if bool then
                val

            else
                0

        goodName name =
            List.any (String.startsWith name) [ "en", "eth", "wl" ]

        hasIpv4 ipv4 =
            ipv4 /= "0.0.0.0"

        isLocalHost ipv4 =
            (ipv4 == "127.0.0.1")
                || (ipv4 == "localhost")
    in
    { info
        | interfaces = List.sortBy networkInterfaceWorthiness info.interfaces
    }


registerHttpError : Http.Error -> Model -> Model
registerHttpError err model =
    { model | errors = Error.httpErrorToString err :: model.errors }


view : Model -> Browser.Document Msg
view model =
    let
        title =
            case model.route of
                Routes.Home ->
                    "Home"

                Routes.DirHome ->
                    "Overview"

                Routes.Dir (Routes.DirPath _ names) ->
                    String.join "/" names

                Routes.Input ->
                    "Input"

                Routes.Remote ->
                    "Remote"

                Routes.IPs ->
                    "IPs"

                Routes.Debug_ ->
                    "Debug"

                Routes.NotFound ->
                    "404"

        body =
            case model.route of
                Routes.Home ->
                    [ Home.view model.roots model.startTime ]

                Routes.DirHome ->
                    Dir.viewHome model.roots DoAction

                Routes.Dir path ->
                    Dir.view model.roots path DoAction

                Routes.Input ->
                    [ map InputMsg Input.view ]

                Routes.Remote ->
                    [ Remote.view DoAction ]

                Routes.IPs ->
                    [ IPs.view model.networkInfo ]

                Routes.Debug_ ->
                    [ div []
                        [ text "Errors:"
                        ]
                    , div [] []
                    ]
                        ++ List.map
                            (\e ->
                                div [] [ text e ]
                            )
                            model.errors

                Routes.NotFound ->
                    [ div []
                        [ text "Page not found."
                        ]
                    , a
                        [ A.href <| Routes.toHref Routes.Home
                        ]
                        [ text "Back home" ]
                    ]
    in
    { title = title ++ " - Pablo TV"
    , body =
        Header.view model.route model.networkInfo model.refreshing NavBack DoAction
            :: body
    }
