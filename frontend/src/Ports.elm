port module Ports exposing (..)


type alias LocalStorageKeyAndData =
    { key : String
    , data : String
    }


port setLocalStorage : LocalStorageKeyAndData -> Cmd msg
