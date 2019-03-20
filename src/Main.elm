module Main exposing (main)

import Browser
import Browser.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    Ball


type alias Ball =
    { x : Int
    , y : Int
    , radius : Int
    , horizSpeed : Int
    }


type Msg
    = OnAnimationFrame Float


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { x = 250
      , y = 250
      , radius = 10
      , horizSpeed = 4
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame timeDelta ->
            ( { model | x = model.x + model.horizSpeed }, Cmd.none )


view : Model -> Svg.Svg Msg
view model =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall model
        , rect
            [ x "480"
            , y "225"
            , width "10"
            , height "50"
            ]
            []
        ]


viewBall : Ball -> Svg.Svg Msg
viewBall { x, y, radius } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r <| String.fromInt radius
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame
