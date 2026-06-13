module Dir exposing (..)

import AggDir
import Browser
import Dict
import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Maybe.Extra as Maybe
import NaturalOrdering
import Routes


viewHome :
    BE.RootDirectories
    -> Browser.Document msg
viewHome roots =
    let
        allFiles =
            Dict.toList roots
                |> List.map (Tuple.mapSecond <| .videoFiles >> Dict.toList)
                |> List.concatMap
                    (\( rootLoc, rootData ) ->
                        List.map
                            (\( fileName, fileData ) ->
                                ( Routes.DirPath rootLoc []
                                , fileName
                                , fileData
                                )
                            )
                            rootData
                    )

        allDirs =
            AggDir.calcForSubDirsOfRoots roots
    in
    view_ Nothing Nothing allFiles allDirs


view :
    BE.RootDirectories
    -> Routes.DirPath
    -> Browser.Document msg
view roots dirPath =
    let
        (Routes.DirPath root pathParts) =
            dirPath

        getDirData : List BE.DirectoryName -> BE.DirectorySubDirs -> Maybe BE.DirectoryData
        getDirData names (BE.DirectorySubDirs dict) =
            case names of
                [] ->
                    Nothing

                [ name ] ->
                    Dict.get name dict

                name :: rest ->
                    Dict.get name dict
                        |> Maybe.map .subDirs
                        |> Maybe.andThen (getDirData rest)

        mDirData : Maybe BE.DirectoryData
        mDirData =
            Dict.get root roots
                |> Maybe.andThen
                    (\rootData ->
                        getDirData pathParts rootData.subDirs
                    )
    in
    case mDirData of
        Nothing ->
            { title = String.join "/" pathParts ++ " - Pablo TV"
            , body =
                [ div [] [ text "Unknown directory :(" ]
                , a
                    [ A.href <| Routes.toHref Routes.Home
                    ]
                    [ text "Back home" ]
                ]
            }

        Just data ->
            let
                videoFiles =
                    Dict.toList data.videoFiles
                        |> List.map
                            (\( fileName, fileData ) ->
                                ( dirPath, fileName, fileData )
                            )

                subDirs =
                    AggDir.calcForSubDirsOf dirPath data
            in
            view_
                (Just dirPath)
                data.image
                videoFiles
                subDirs


view_ :
    Maybe Routes.DirPath
    -> Maybe BE.Image
    -> List ( Routes.DirPath, BE.VideoFileName, BE.VideoFileData )
    -> List AggDir.AggDirInfo
    -> Browser.Document msg
view_ dirPath dirImage unsortedFiles unsortedSubDirs =
    let
        image =
            dirPath
                |> Maybe.map
                    (\_ -> dirImage :: List.map .image unsortedSubDirs)
                |> Maybe.andThen Maybe.orList

        files =
            NaturalOrdering.sortBy (\( _, name, _ ) -> name) unsortedFiles

        subDirs =
            NaturalOrdering.sortBy .name unsortedSubDirs
    in
    { title =
        case dirPath of
            Nothing ->
                "Overview"

            Just (Routes.DirPath _ pathParts) ->
                String.join "/" pathParts
    , body =
        [ case image of
            Nothing ->
                text ""

            Just (BE.Image i) ->
                img [ A.src <| "/image/" ++ i.cached ] []
        , text "files"
        , ol [] <|
            List.map (\( _, name, _ ) -> li [] [ text name ]) files
        , text "dirs"
        , ol [] <|
            List.map (\d -> li [] [ text d.name ]) subDirs
        ]
    }
