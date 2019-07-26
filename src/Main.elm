module Main exposing (main)

import Browser
import Browser.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { ball : Ball
    , paddle : Paddle
    }


type alias Ball =
    { x : Int
    , y : Int
    , radius : Int
    , horizSpeed : Int
    }


type alias Paddle =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type Msg
    = OnAnimationFrame Float


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { ball =
            initBall
      , paddle = initPaddle
      }
    , Cmd.none
    )


initBall : Ball
initBall =
    { x = 250
    , y = 250
    , radius = 10
    , horizSpeed = 4
    }


initPaddle : Paddle
initPaddle =
    { x = 480
    , y = 225
    , width = 10
    , height = 50
    }


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
            let
                ball =
                    model.ball

                shouldBounce =
                    shouldBallBounce model.paddle model.ball
                        |> Debug.log "shouldBounce"

                updatedBall =
                    { ball | x = ball.x + ball.horizSpeed }
            in
            ( { model | ball = updatedBall }, Cmd.none )


shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball =
    (ball.x + ball.radius >= paddle.x)
        && (ball.y >= paddle.y - 50 // 2)
        && (ball.y <= paddle.y + 50 // 2)


view : Model -> Svg.Svg Msg
view { ball, paddle } =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        , viewPaddle paddle
        ]


viewBall : Ball -> Svg.Svg Msg
viewBall { x, y, radius } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r <| String.fromInt radius
        ]
        []


viewPaddle : Paddle -> Svg.Svg Msg
viewPaddle paddle =
    rect
        [ x <| String.fromInt paddle.x
        , y <| String.fromInt paddle.y
        , width <| String.fromInt paddle.width
        , height <| String.fromInt paddle.height
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame
