module Home exposing (..)

import Generated.Backend exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Time
import Time.Extra as Time


type alias AggDirInfo =
    { name : DirectoryName
    , path : RawWebPath
    , image : Maybe Image
    , lastAdded : Time.Posix
    , lastWatched : Time.Posix
    , videoFileCount : Int
    , playedVideoFileCount : Int
    }


viewRow : List AggDirInfo -> Html msg
viewRow dirs =
    div [ A.class "row" ] <|
        List.map viewPoster dirs


viewPoster : AggDirInfo -> Html msg
viewPoster dir =
    a [ A.class "el", A.href "todo" ]
        [ img
            [ A.src <|
                "image/"
                    ++ (case dir.image of
                            Just (Image img) ->
                                img.cached

                            Nothing ->
                                "missing"
                       )
            , A.alt dir.name
            , A.attribute "loading" "lazy"
            ]
            []
        , div [ A.class "overlay" ]
            [ i [ A.class "fa-solid fa-play" ] []
            , text <|
                String.fromInt dir.playedVideoFileCount
                    ++ "/"
                    ++ String.fromInt dir.videoFileCount
            ]
        ]
