module LocalStorage exposing (..)

import Generated.Backend as BE
import Json.Encode as E
import Ports


saveRoots : BE.RootDirectories -> Cmd msg
saveRoots roots =
    Ports.setLocalStorage
        { key = "roots"
        , data = E.encode 0 <| BE.jsonEncRootDirectories roots
        }


saveNetworkInfo : BE.NetworkInfo -> Cmd msg
saveNetworkInfo networkInfo =
    Ports.setLocalStorage
        { key = "network-info"
        , data = E.encode 0 <| BE.jsonEncNetworkInfo networkInfo
        }
