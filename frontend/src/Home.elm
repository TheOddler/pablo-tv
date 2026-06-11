module Home exposing (..)

import AggDir exposing (AggDirInfo)
import Generated.Backend exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Random


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


view : RootDirectories -> Int -> Html msg
view roots startTime =
    let
        aggInfos =
            AggDir.calcForSubDirsOfRoots roots

        seed1 =
            Random.initialSeed startTime

        seed2 =
            Random.initialSeed <| startTime + 42

        row filter sorting =
            viewRow <|
                AggDir.filterAndSort filter sorting aggInfos
    in
    div [ A.id "home-container" ]
        [ h1 [] [ text "Watching" ]
        , row AggDir.Watching AggDir.RecentlyWatched
        , h1 [] [ text "New" ]
        , row AggDir.NothingWatched AggDir.RecentlyAdded
        , h1 [] [ text "Random" ]
        , row AggDir.NothingWatched (AggDir.Shuffled seed1)
        , h1 [] [ text "Recently Added" ]
        , row AggDir.Unfiltered AggDir.RecentlyAdded
        , h1 [] [ text "Random (All)" ]
        , row AggDir.Unfiltered (AggDir.Shuffled seed2)
        , h1 [] [ text "Recently Finished" ]
        , row AggDir.FullyWatched AggDir.RecentlyWatched
        ]
