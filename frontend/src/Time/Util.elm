module Time.Util exposing (..)

import List.Extra as List
import Time


max : Time.Posix -> Time.Posix -> Time.Posix
max x y =
    if Time.posixToMillis x >= Time.posixToMillis y then
        x

    else
        y


maximum : List Time.Posix -> Maybe Time.Posix
maximum =
    List.maximumBy Time.posixToMillis
