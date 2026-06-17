module Main exposing (Msg(..), main, update, view)

import Browser
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
import Json.Decode as D
import LocalStorage
import Platform.Cmd as Cmd
import Routes
import Time
import Url


type alias Model =
    { startTime : Int
    , roots : BE.RootDirectories
    , networkInfo : BE.NetworkInfo
    , navKey : Nav.Key
    , route : Routes.Route
    , errors : List String
    }


type alias Flags =
    { startTime : Int
    , roots : Maybe String
    , networkInfo : Maybe String
    }


type Msg
    = GetDirsUpdate
    | GotDirsUpdateResult (Result Http.Error BE.RootDirectories)
    | NetworkInfoUpdate (Result Http.Error BE.NetworkInfo)
    | GotUrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | NavBack Int
    | DoAction BE.Action
    | GotActionResult (Result Http.Error ())


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
        oneSecond =
            1000

        refreshData _ =
            GetDirsUpdate
    in
    Time.every oneSecond refreshData


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
      , networkInfo = networkInfo -- Network info is sorted when I save it to the local storage, so no need to sort again here
      , navKey = navKey
      , route = Routes.parse (Dict.keys roots) url
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
                    registerHttpError err model
    in
    case msg of
        GetDirsUpdate ->
            ( model
            , BE.getApiData GotDirsUpdateResult
            )

        GotDirsUpdateResult res ->
            onHttpSuccess (Debug.log "DirsUpdate" res) <|
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
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Routes.parse (Dict.keys model.roots) url }
            , Cmd.none
            )

        NavBack steps ->
            ( model
            , Nav.back model.navKey steps
            )

        DoAction action ->
            ( model
            , BE.postApiAction action GotActionResult
            )

        GotActionResult result ->
            onHttpSuccess result <|
                \() ->
                    ( model
                    , BE.getApiData GotDirsUpdateResult
                    )


sortNetworkInfo : BE.NetworkInfo -> BE.NetworkInfo
sortNetworkInfo info =
    let
        networkInterfaceWorthiness : BE.SimpleNetworkInterface -> Int
        networkInterfaceWorthiness { name, ipv4 } =
            List.sum
                [ hasIpv4 ipv4 |> countAs 2
                , goodName name |> countAs 1
                , isLocalHost ipv4 |> countAs -10
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


registerHttpError : Http.Error -> Model -> ( Model, Cmd Msg )
registerHttpError err model =
    ( { model | errors = Error.httpErrorToString err :: model.errors }
    , Cmd.none
    )


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
                    "Directory " ++ String.join "/" names

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

        todo pageName =
            [ div []
                [ text <| "Page TODO: " ++ pageName
                ]
            , a
                [ A.href <| Routes.toHref Routes.Home
                ]
                [ text "Back home" ]
            ]

        body =
            case model.route of
                Routes.Home ->
                    [ Home.view model.roots model.startTime ]

                Routes.DirHome ->
                    Dir.viewHome model.roots DoAction

                Routes.Dir path ->
                    Dir.view model.roots path DoAction

                Routes.Input ->
                    todo "viewInput"

                Routes.Remote ->
                    todo "viewRemote"

                Routes.IPs ->
                    todo "viewIPs"

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
        Header.view model.route model.networkInfo NavBack DoAction
            :: body
    }
