module Main exposing (main)

import Browser
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


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , view = \_ -> view ball
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view : Ball -> Svg.Svg ()
view ball_ =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball_
        ]


viewBall : Ball -> Svg.Svg msg
viewBall { x, y } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "10"
        ]
        []
