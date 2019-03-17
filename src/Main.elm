module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Ball =
    { x : Int
    , y : Int
    }


ball =
    { x = 250
    , y = 250
    }


main =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        ]


viewBall : Ball -> Svg.Svg msg
viewBall { x, y } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "10"
        ]
        []
