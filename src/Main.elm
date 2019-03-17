module Main exposing (main)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    Ball


type alias Ball =
    { x : Int
    , y : Int
    }


ball =
    { x = 250
    , y = 250
    }


main : Program () Model ()
main =
    Browser.element
        { init = \_ -> ( ball, Cmd.none )
        , view = view
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Svg.Svg ()
view model =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall model
        ]


viewBall : Ball -> Svg.Svg msg
viewBall { x, y } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "10"
        ]
        []
