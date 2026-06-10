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
    { aggDirName : DirectoryName
    , aggDirPath : RawWebPath
    , aggDirImage : Maybe Image
    , aggDirLastAdded : Time.Posix
    , aggDirLastWatched : Time.Posix
    , aggDirVideoFileCount : Int
    , aggDirPlayedVideoFileCount : Int
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
                    ++ (case dir.aggDirImage of
                            Just (Image img) ->
                                img.cached

                            Nothing ->
                                "missing"
                       )
            , A.alt dir.aggDirName
            , A.attribute "loading" "lazy"
            ]
            []
        , div [ A.class "overlay" ]
            [ i [ A.class "fa-solid fa-play" ] []
            , text <|
                String.fromInt dir.aggDirPlayedVideoFileCount
                    ++ "/"
                    ++ String.fromInt dir.aggDirVideoFileCount
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
                                | aggDirImage = Maybe.or agg.aggDirImage next.image
                                , aggDirLastAdded =
                                    Time.max agg.aggDirLastAdded thisLastAdded
                                , aggDirLastWatched =
                                    Time.max agg.aggDirLastWatched thisLastWatched
                                , aggDirVideoFileCount =
                                    agg.aggDirVideoFileCount + thisVideoFileCount
                                , aggDirPlayedVideoFileCount =
                                    agg.aggDirPlayedVideoFileCount + thisPlayedVideoFileCount
                            }
                    in
                    go updated <| rest ++ Dict.values (subDirsUnwrapped next)
    in
    go
        { aggDirName = name
        , aggDirPath = path
        , aggDirImage = info.image
        , aggDirLastAdded = calcLastAdded <| Dict.values info.videoFiles
        , aggDirLastWatched = calcLastWatched <| Dict.values info.videoFiles
        , aggDirVideoFileCount = Dict.size info.videoFiles
        , aggDirPlayedVideoFileCount = countPlayed <| Dict.values info.videoFiles
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
                        a.aggDirPlayedVideoFileCount
                            > 0
                            && a.aggDirPlayedVideoFileCount
                            < a.aggDirVideoFileCount

                NothingWatched ->
                    \a ->
                        a.aggDirPlayedVideoFileCount == 0

                FullyWatched ->
                    \a -> a.aggDirPlayedVideoFileCount == a.aggDirVideoFileCount

                Unfiltered ->
                    \_ -> True

        sort : List AggDirInfo -> List AggDirInfo
        sort =
            case sorting of
                RecentlyAdded ->
                    List.sortBy (negate << Time.posixToMillis << .aggDirLastWatched)

                RecentlyWatched ->
                    List.sortBy (negate << Time.posixToMillis << .aggDirLastWatched)

                Shuffled seed ->
                    \a ->
                        Random.step (Random.List.shuffle a) seed
                            |> Tuple.first
    in
    List.filter filterFunc list
        |> sort
