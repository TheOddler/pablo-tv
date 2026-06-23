module String.Util exposing (..)


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
