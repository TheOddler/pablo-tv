module IPs exposing (..)

import Generated.Backend as BE
import Html exposing (..)
import Html.Attributes as A


view : BE.NetworkInfo -> Html msg
view info =
    div
        [ A.id "ips-container"
        ]
        [ p [ A.class "large" ]
            [ text <| "Port: " ++ String.fromInt info.port_
            ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "IPv4" ]
                    ]
                ]
            , tbody [] <|
                List.map
                    (\i ->
                        tr []
                            [ td [] [ text i.name ]
                            , td [] [ text i.ipv4 ]
                            ]
                    )
                    info.interfaces
            ]
        ]
