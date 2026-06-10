module Home exposing (..)

import Dict
import Generated.Backend exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Maybe.Extra as Maybe
import Random
import Random.List
import Time
import Time.Extra as Time
import Tuple


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


{-| If there are files in the root, those are ignored here.
-}
calcAggInfos : RootDirectories -> List AggDirInfo
calcAggInfos roots =
    let
        aggOneRootDir ( rootLoc, subDirs ) =
            Dict.toList subDirs
                |> List.map
                    (\( dirName, dirData ) ->
                        calcAggInfo
                            (rootLoc ++ "/" ++ dirName)
                            dirName
                            dirData
                    )
    in
    Dict.toList roots
        |> List.map (Tuple.mapSecond subDirsUnwrapped)
        |> List.concatMap aggOneRootDir


calcAggInfo : RawWebPath -> DirectoryName -> DirectoryData -> AggDirInfo
calcAggInfo path name info =
    let
        calcLastAdded : List VideoFileData -> Time.Posix
        calcLastAdded files =
            List.map .added files
                |> Time.maximum
                |> Maybe.withDefault (Time.millisToPosix 0)

        calcLastWatched : List VideoFileData -> Time.Posix
        calcLastWatched files =
            List.map .watched files
                |> Maybe.values
                |> Time.maximum
                |> Maybe.withDefault (Time.millisToPosix 0)

        countPlayed : List VideoFileData -> Int
        countPlayed files =
            List.map .watched files
                |> Maybe.values
                |> List.length

        go : AggDirInfo -> List DirectoryData -> AggDirInfo
        go agg toCheck =
            case toCheck of
                [] ->
                    agg

                next :: rest ->
                    let
                        thisLastAdded =
                            calcLastAdded <| Dict.values next.videoFiles

                        thisLastWatched =
                            calcLastWatched <| Dict.values next.videoFiles

                        thisVideoFileCount =
                            Dict.size next.videoFiles

                        thisPlayedVideoFileCount =
                            countPlayed <| Dict.values next.videoFiles

                        updated =
                            { agg
                                | image = Maybe.or agg.image next.image
                                , lastAdded =
                                    Time.max agg.lastAdded thisLastAdded
                                , lastWatched =
                                    Time.max agg.lastWatched thisLastWatched
                                , videoFileCount =
                                    agg.videoFileCount + thisVideoFileCount
                                , playedVideoFileCount =
                                    agg.playedVideoFileCount + thisPlayedVideoFileCount
                            }
                    in
                    go updated <| rest ++ Dict.values (subDirsUnwrapped next)
    in
    go
        { name = name
        , path = path
        , image = info.image
        , lastAdded = calcLastAdded <| Dict.values info.videoFiles
        , lastWatched = calcLastWatched <| Dict.values info.videoFiles
        , videoFileCount = Dict.size info.videoFiles
        , playedVideoFileCount = countPlayed <| Dict.values info.videoFiles
        }
        (subDirsUnwrapped info |> Dict.values)


type Filter
    = Watching
    | NothingWatched
    | FullyWatched
    | Unfiltered


type Sorting
    = RecentlyAdded
    | RecentlyWatched
    | Shuffled Random.Seed


filterAndSort : Filter -> Sorting -> List AggDirInfo -> List AggDirInfo
filterAndSort filter sorting list =
    let
        filterFunc : AggDirInfo -> Bool
        filterFunc =
            case filter of
                Watching ->
                    \a ->
                        a.playedVideoFileCount
                            > 0
                            && a.playedVideoFileCount
                            < a.videoFileCount

                NothingWatched ->
                    \a ->
                        a.playedVideoFileCount == 0

                FullyWatched ->
                    \a -> a.playedVideoFileCount == a.videoFileCount

                Unfiltered ->
                    \_ -> True

        sort : List AggDirInfo -> List AggDirInfo
        sort =
            case sorting of
                RecentlyAdded ->
                    List.sortBy (negate << Time.posixToMillis << .lastWatched)

                RecentlyWatched ->
                    List.sortBy (negate << Time.posixToMillis << .lastWatched)

                Shuffled seed ->
                    \a ->
                        Random.step (Random.List.shuffle a) seed
                            |> Tuple.first
    in
    List.filter filterFunc list
        |> sort
