port module Ports exposing (..)

import Generated.Backend as BE
import Json.Encode as E


saveRoots : BE.RootDirectories -> Cmd msg
saveRoots roots =
    saveRootsPort <| E.encode 2 <| BE.jsonEncRootDirectories roots


port saveRootsPort : String -> Cmd msg
