module String.Util exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe


splitAny : List String -> String -> List String
splitAny allSeparators string =
    let
        go seps agg =
            case seps of
                [] ->
                    agg

                sep :: rest ->
                    List.concatMap (String.split sep) agg
                        |> go rest
    in
    go allSeparators [ string ]


type alias StringComponents =
    { commonPrefix : String
    , uniqueMiddles : List String
    , commonSuffix : String
    }


findCommonPreAndSuffix : List String -> StringComponents
findCommonPreAndSuffix strings =
    let
        splitFileNames : List (List String)
        splitFileNames =
            List.map (splitAny [ ".", " ", "_", "/" ]) strings

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
            List.map getUniqueMiddle strings
    in
    { commonPrefix = commonPrefix
    , uniqueMiddles = uniqueMiddles
    , commonSuffix = commonSuffix
    }
