module DirData.Util exposing (..)

import Dict
import Generated.Backend as BE


foldDepthFirst :
    (BE.DirectoryData -> agg -> agg)
    -> BE.DirectoryData
    -> agg
    -> agg
foldDepthFirst func dir agg =
    let
        aggUpdatedWithThisDir =
            func dir agg

        subDirs =
            Dict.values <| BE.subDirsUnwrapped dir

        depthFirstFunc =
            foldDepthFirst func
    in
    List.foldl depthFirstFunc aggUpdatedWithThisDir subDirs
