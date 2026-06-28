module Dir exposing (..)

import AggDir
import Dict
import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Html.Util exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import NaturalOrdering
import Routes
import String.Util as String


viewHome :
    BE.RootDirectories
    -> (BE.Action -> msg)
    -> List (Html msg)
viewHome roots doAction =
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
    view_
        Nothing
        Nothing
        allFiles
        allDirs
        doAction


view :
    BE.RootDirectories
    -> Routes.DirPath
    -> (BE.Action -> msg)
    -> List (Html msg)
view roots dirPath doAction =
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

        getNearestParentImage : List BE.DirectoryName -> Maybe BE.Image -> BE.DirectorySubDirs -> Maybe BE.Image
        getNearestParentImage names foundImg (BE.DirectorySubDirs dict) =
            case names of
                [] ->
                    foundImg

                [ _ ] ->
                    foundImg

                name :: rest ->
                    Dict.get name dict
                        |> Maybe.andThen
                            (\data ->
                                let
                                    newFoundImg =
                                        data.image |> Maybe.or foundImg
                                in
                                getNearestParentImage rest newFoundImg data.subDirs
                            )

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
            [ div [] [ text "Unknown directory :(" ]
            , a
                [ A.href <| Routes.toHref Routes.Home
                ]
                [ text "Back home" ]
            ]

        Just data ->
            let
                image =
                    Dict.get root roots
                        |> Maybe.andThen
                            (\rootData ->
                                case data.image of
                                    Just i ->
                                        Just i

                                    Nothing ->
                                        getNearestParentImage pathParts Nothing rootData.subDirs
                            )

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
                image
                videoFiles
                subDirs
                doAction


type alias NiceFileName =
    { commonPrefix : String
    , uniqueMiddle : String
    , commonSuffix : String
    }


view_ :
    Maybe Routes.DirPath
    -> Maybe BE.Image
    -> List ( Routes.DirPath, BE.VideoFileName, BE.VideoFileData )
    -> List AggDir.AggDirInfo
    -> (BE.Action -> msg)
    -> List (Html msg)
view_ mDirPath dirImage unsortedFiles unsortedSubDirs doAction =
    let
        image =
            mDirPath
                |> Maybe.map
                    (\_ -> dirImage :: List.map .image unsortedSubDirs)
                |> Maybe.andThen Maybe.orList

        files : List ( Routes.DirPath, BE.VideoFileName, BE.VideoFileData )
        files =
            NaturalOrdering.sortBy (\( _, name, _ ) -> name) unsortedFiles

        filesWithNiceName : List ( Routes.DirPath, ( BE.VideoFileName, NiceFileName ), BE.VideoFileData )
        filesWithNiceName =
            let
                namesWithoutExtension : List BE.VideoFileName
                namesWithoutExtension =
                    List.map (\( _, name, _ ) -> name) files
                        |> List.map (String.split ".")
                        |> List.map (\splits -> List.take (List.length splits - 1) splits)
                        |> List.map (String.join " ")

                splitFileNames : List (List String)
                splitFileNames =
                    namesWithoutExtension
                        -- We already replaced . with spaces above
                        |> List.map (String.splitAny [ " ", "_", "/" ])

                findCommonPrefix : List (List String) -> List String
                findCommonPrefix xs =
                    case Maybe.combine (List.map List.uncons xs) of
                        Nothing ->
                            []

                        Just [] ->
                            []

                        Just [ _ ] ->
                            []

                        Just (( head_, tail_ ) :: rest) ->
                            if List.all ((\a -> a == head_) << Tuple.first) rest then
                                head_ :: findCommonPrefix (tail_ :: List.map Tuple.second rest)

                            else
                                []

                commonPrefix : String
                commonPrefix =
                    String.join " " <| findCommonPrefix splitFileNames

                findCommonSuffix : List (List String) -> List String
                findCommonSuffix =
                    List.reverse << findCommonPrefix << List.map List.reverse

                commonSuffix : String
                commonSuffix =
                    String.join " " <| findCommonSuffix splitFileNames

                getUniqueMiddle : String -> String
                getUniqueMiddle n =
                    String.slice
                        (String.length commonPrefix)
                        (String.length n - String.length commonSuffix)
                        n

                uniqueMiddles : List String
                uniqueMiddles =
                    List.map getUniqueMiddle namesWithoutExtension

                mkNiceName : String -> NiceFileName
                mkNiceName middle =
                    { commonPrefix = commonPrefix
                    , uniqueMiddle = middle
                    , commonSuffix = commonSuffix
                    }

                niceNames =
                    List.map mkNiceName uniqueMiddles
            in
            List.zip files niceNames
                |> List.map (\( ( path, name, data ), middle ) -> ( path, ( name, middle ), data ))

        subDirs =
            NaturalOrdering.sortBy .name unsortedSubDirs

        ( title, subTitle ) =
            case mDirPath |> Maybe.andThen (\(Routes.DirPath _ p) -> List.last p) of
                Nothing ->
                    ( "Videos", "" )

                Just dirName ->
                    String.indexes "(" dirName
                        |> List.head
                        |> Maybe.map
                            (\i ->
                                ( String.slice 0 i dirName
                                , String.slice i (String.length dirName) dirName
                                )
                            )
                        |> Maybe.withDefault ( dirName, "" )

        iff bool a =
            if bool then
                a

            else
                text ""

        ifJust maybe f =
            Maybe.map f maybe
                |> Maybe.withDefault (text "")

        ifAndJust bool maybe f =
            if bool then
                ifJust maybe f

            else
                text ""
    in
    [ div [ A.id "dir-container" ]
        [ -- Image
          case image of
            Nothing ->
                text ""

            Just (BE.Image i) ->
                img
                    [ A.src <| "/image/" ++ i.cached
                    , A.alt "Poster"
                    , A.class "header"
                    ]
                    []

        -- Title
        , h1 []
            [ text title
            , if subTitle == "" then
                text ""

              else
                span [ A.class "sub-title" ] [ text subTitle ]
            ]

        -- Top buttons
        , iff (List.length files > 1 || List.length subDirs > 0) <|
            div [ A.class "section" ]
                [ div [ A.class "row white" ]
                    [ ifJust mDirPath <|
                        \dirPath ->
                            button
                                [ A.class "like-link large name"
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionPlayPath
                                            { path = Routes.toRawWebPath dirPath }
                                ]
                                [ i [ A.class "fa-solid fa-play" ] []
                                , span [] [ text "Play All" ]
                                ]
                    , ifAndJust (List.length files > 0) mDirPath <|
                        \dirPath ->
                            button
                                [ A.class "like-link"
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionMarkAsWatched
                                            { path = Routes.toRawWebPath dirPath }
                                ]
                                [ i [ A.class "fa-regular fa-eye" ] [] ]
                    , ifAndJust (List.length files > 0) mDirPath <|
                        \dirPath ->
                            button
                                [ A.class "like-link"
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionMarkAsUnwatched
                                            { path = Routes.toRawWebPath dirPath }
                                ]
                                [ i [ A.class "fa-regular fa-eye-slash" ] [] ]
                    ]
                ]

        -- Files
        , iff (List.length files > 0) <|
            div [ A.class "section" ] <|
                List.map
                    (\( dirPath, ( rawName, niceName ), data ) ->
                        let
                            fullNiceName =
                                niceName.commonPrefix
                                    ++ " "
                                    ++ niceName.uniqueMiddle
                                    ++ " "
                                    ++ niceName.commonSuffix
                        in
                        div
                            [ A.class "row"
                            , A.class <|
                                case data.watched of
                                    Nothing ->
                                        "unwatched"

                                    Just _ ->
                                        "watched"
                            ]
                            [ button
                                [ A.class "name like-link"
                                , A.title <| "Play: " ++ fullNiceName
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionPlayPath
                                            { path = Routes.toRawWebPathFile dirPath rawName }
                                ]
                                [ i [ A.class "fa-solid fa-play" ] []
                                , if niceName.commonPrefix == "" then
                                    text ""

                                  else
                                    span [ A.class "prefix" ] [ nbsp, text niceName.commonPrefix ]
                                , span [ A.class "unique" ] [ text niceName.uniqueMiddle ]
                                , if niceName.commonSuffix == "" then
                                    text ""

                                  else
                                    span [ A.class "suffix" ] [ nbsp, text niceName.commonSuffix ]
                                ]
                            , case data.watched of
                                Nothing ->
                                    i [] []

                                Just _ ->
                                    i [ A.class "fa-solid fa-check" ] []
                            , button
                                [ A.class "like-link"
                                , A.title <| "Mark as watched: " ++ fullNiceName
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionMarkAsWatched
                                            { path = Routes.toRawWebPathFile dirPath rawName }
                                ]
                                [ i [ A.class "fa-regular fa-eye" ] [] ]
                            , button
                                [ A.class "like-link"
                                , A.title <| "Mark as unwatched: " ++ fullNiceName
                                , E.onClick <|
                                    doAction <|
                                        BE.ActionMarkAsUnwatched
                                            { path = Routes.toRawWebPathFile dirPath rawName }
                                ]
                                [ i [ A.class "fa-regular fa-eye-slash" ] [] ]
                            ]
                    )
                    filesWithNiceName

        -- Sub directories
        , iff (List.length subDirs > 0) <|
            div [ A.class "section" ] <|
                List.map
                    (\aggDir ->
                        div
                            [ A.class "row"
                            , A.class <|
                                if aggDir.videoFileCount == aggDir.playedVideoFileCount then
                                    "watched"

                                else if aggDir.playedVideoFileCount > 0 then
                                    "watching"

                                else
                                    "unwatched"
                            ]
                            [ a
                                [ A.href <| Routes.toHref <| Routes.Dir aggDir.path
                                , A.class "name"
                                ]
                                [ i [ A.class "fa-regular fa-folder-open" ] []
                                , nbsp
                                , span [] [ text aggDir.name ]
                                ]
                            , div [] [] -- Just there to make the css easier, it's in place of the watched file tick
                            , i [ A.class "#{dirIcon dir}" ] []
                            , span []
                                [ text <| String.fromInt aggDir.playedVideoFileCount
                                , text "/"
                                , text <| String.fromInt aggDir.videoFileCount
                                ]
                            ]
                    )
                    subDirs

        -- A bit of extra space at the bottom
        , div [ A.class "bottom-spacer" ] [ nbsp ]
        ]
    ]
