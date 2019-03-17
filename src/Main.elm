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
        [ viewBall 250 250
        ]


viewBall : Int -> Int -> Svg.Svg msg
viewBall x y =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "10"
        ]
        []
