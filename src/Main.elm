module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall
        ]


viewBall : Svg msg
viewBall =
    circle
        [ cx "250"
        , cy "250"
        , r "10"
        ]
        []
