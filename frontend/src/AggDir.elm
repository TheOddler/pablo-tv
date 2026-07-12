module AggDir exposing (..)

import Dict
import DirData.Util as DirData
import Generated.Backend as BE
import Html exposing (..)
import Maybe.Extra as Maybe
import Random
import Random.List
import Routes
import Time
import Time.Util as Time
import Tuple


type alias AggDirInfo =
    { name : BE.DirectoryName
    , path : Routes.DirPath
    , image : Maybe BE.Image
    , lastAdded : Time.Posix
    , lastWatched : Time.Posix
    , videoFileCount : Int
    , playedVideoFileCount : Int
    }


calcForSubDirsOf :
    Routes.DirPath
    -> { a | subDirs : BE.DirectorySubDirs }
    -> List AggDirInfo
calcForSubDirsOf pathOfDir dirData =
    BE.subDirsUnwrapped dirData
        |> Dict.toList
        |> List.map
            (\( subDirName, subDirData ) ->
                calcAggInfo
                    pathOfDir
                    subDirName
                    subDirData
            )


calcForSubDirsOfRoots : BE.RootDirectories -> List AggDirInfo
calcForSubDirsOfRoots roots =
    Dict.toList roots
        |> List.concatMap
            (\( rootLoc, rootData ) ->
                calcForSubDirsOf (Routes.DirPath rootLoc []) rootData
            )


calcAggInfo : Routes.DirPath -> BE.DirectoryName -> BE.DirectoryData -> AggDirInfo
calcAggInfo (Routes.DirPath root parentPath) name info =
    let
        calcLastAdded : List BE.VideoFileData -> Time.Posix
        calcLastAdded files =
            List.map .added files
                |> Time.maximum
                |> Maybe.withDefault (Time.millisToPosix 0)

        calcLastWatched : List BE.VideoFileData -> Time.Posix
        calcLastWatched files =
            List.map .watched files
                |> Maybe.values
                |> Time.maximum
                |> Maybe.withDefault (Time.millisToPosix 0)

        countPlayed : List BE.VideoFileData -> Int
        countPlayed files =
            List.map .watched files
                |> Maybe.values
                |> List.length

        startAggDirInfo =
            { name = name
            , path = Routes.DirPath root <| parentPath ++ [ name ]
            , image = info.image
            , lastAdded = Time.millisToPosix 0
            , lastWatched = Time.millisToPosix 0
            , videoFileCount = 0
            , playedVideoFileCount = 0
            }

        addToDirInfo : BE.DirectoryData -> AggDirInfo -> AggDirInfo
        addToDirInfo dd agg =
            { agg
                | image = Maybe.or agg.image dd.image
                , lastAdded =
                    Time.max agg.lastAdded <|
                        calcLastAdded (Dict.values dd.videoFiles)
                , lastWatched =
                    Time.max agg.lastWatched <|
                        calcLastWatched (Dict.values dd.videoFiles)
                , videoFileCount =
                    agg.videoFileCount
                        + Dict.size dd.videoFiles
                , playedVideoFileCount =
                    agg.playedVideoFileCount
                        + countPlayed (Dict.values dd.videoFiles)
            }
    in
    DirData.foldDepthFirst addToDirInfo info startAggDirInfo


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
                    List.sortBy (negate << Time.posixToMillis << .lastAdded)

                RecentlyWatched ->
                    List.sortBy (negate << Time.posixToMillis << .lastWatched)

                Shuffled seed ->
                    \a ->
                        Random.step (Random.List.shuffle a) seed
                            |> Tuple.first
    in
    doFilter list |> sort
