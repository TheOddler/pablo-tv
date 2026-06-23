module AggDir exposing (..)

import Dict
import Generated.Backend exposing (..)
import Html exposing (..)
import Maybe.Extra as Maybe
import Random
import Random.List
import Routes
import Time
import Time.Util as Time
import Tuple


type alias AggDirInfo =
    { name : DirectoryName
    , path : Routes.DirPath
    , image : Maybe Image
    , lastAdded : Time.Posix
    , lastWatched : Time.Posix
    , videoFileCount : Int
    , playedVideoFileCount : Int
    }


calcForSubDirsOf :
    Routes.DirPath
    -> { a | subDirs : DirectorySubDirs }
    -> List AggDirInfo
calcForSubDirsOf pathOfDir dirData =
    subDirsUnwrapped dirData
        |> Dict.toList
        |> List.map
            (\( subDirName, subDirData ) ->
                calcAggInfo
                    pathOfDir
                    subDirName
                    subDirData
            )


calcForSubDirsOfRoots : RootDirectories -> List AggDirInfo
calcForSubDirsOfRoots roots =
    Dict.toList roots
        |> List.concatMap
            (\( rootLoc, rootData ) ->
                calcForSubDirsOf (Routes.DirPath rootLoc []) rootData
            )


calcAggInfo : Routes.DirPath -> DirectoryName -> DirectoryData -> AggDirInfo
calcAggInfo (Routes.DirPath root parentPath) name info =
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
        , path = Routes.DirPath root <| parentPath ++ [ name ]
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
        doFilter : List AggDirInfo -> List AggDirInfo
        doFilter =
            case filter of
                Watching ->
                    List.filter <|
                        \a ->
                            (a.playedVideoFileCount > 0)
                                && (a.playedVideoFileCount < a.videoFileCount)

                NothingWatched ->
                    List.filter <|
                        \a -> a.playedVideoFileCount == 0

                FullyWatched ->
                    List.filter <|
                        \a -> a.playedVideoFileCount == a.videoFileCount

                Unfiltered ->
                    \l -> l

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
    doFilter list |> sort
