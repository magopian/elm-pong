module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { ball : Ball
    , rightPaddle : Paddle
    , leftPaddle : Paddle
    , rightPaddleMovement : PaddleMovement
    , leftPaddleMovement : PaddleMovement
    , gameStatus : GameStatus
    }


type alias Ball =
    { x : Int
    , y : Int
    , radius : Int
    , horizSpeed : Int
    , vertSpeed : Int
    }


type Paddle
    = RightPaddle PaddleInfo
    | LeftPaddle PaddleInfo


type alias PaddleInfo =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type PaddleMovement
    = MovingUp
    | MovingDown
    | NotMoving


type Player
    = LeftPlayer
    | RightPlayer


type GameStatus
    = NoWinner
    | Winner Player


type Msg
    = OnAnimationFrame Float
    | KeyDown PlayerAction
    | KeyUp PlayerAction


type PlayerAction
    = RightPaddleUp
    | RightPaddleDown
    | LeftPaddleUp
    | LeftPaddleDown


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { ball = initBall
      , rightPaddle = RightPaddle <| initPaddle 480
      , leftPaddle = LeftPaddle <| initPaddle 10
      , rightPaddleMovement = NotMoving
      , leftPaddleMovement = NotMoving
      , gameStatus = NoWinner
      }
    , Cmd.none
    )


initBall : Ball
initBall =
    { x = 250
    , y = 250
    , radius = 10
    , horizSpeed = 4
    , vertSpeed = 2
    }


initPaddle : Int -> PaddleInfo
initPaddle initialX =
    { x = initialX
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
                    shouldBallBounce model.rightPaddle model.ball
                        || shouldBallBounce model.leftPaddle model.ball

                horizSpeed =
                    if shouldBounce then
                        ball.horizSpeed * -1

                    else
                        ball.horizSpeed

                shouldBounceVertically =
                    shouldBallBounceVertically model.ball

                vertSpeed =
                    if shouldBounceVertically then
                        ball.vertSpeed * -1

                    else
                        ball.vertSpeed

                updatedBall =
                    { ball
                        | x = ball.x + horizSpeed
                        , y = ball.y + vertSpeed
                        , horizSpeed = horizSpeed
                        , vertSpeed = vertSpeed
                    }

                updatedRightPaddle =
                    updatePaddle model.rightPaddleMovement model.rightPaddle

                updatedLeftPaddle =
                    updatePaddle model.leftPaddleMovement model.leftPaddle

                gameStatus =
                    case maybeWinner updatedBall of
                        Nothing ->
                            NoWinner

                        Just player ->
                            Winner player
            in
            ( { model
                | ball = updatedBall
                , rightPaddle = updatedRightPaddle
                , leftPaddle = updatedLeftPaddle
                , gameStatus = gameStatus
              }
            , Cmd.none
            )

        KeyDown playerAction ->
            case playerAction of
                RightPaddleUp ->
                    ( { model | rightPaddleMovement = MovingUp }
                    , Cmd.none
                    )

                RightPaddleDown ->
                    ( { model | rightPaddleMovement = MovingDown }
                    , Cmd.none
                    )

                LeftPaddleUp ->
                    ( { model | leftPaddleMovement = MovingUp }
                    , Cmd.none
                    )

                LeftPaddleDown ->
                    ( { model | leftPaddleMovement = MovingDown }
                    , Cmd.none
                    )

        KeyUp playerAction ->
            case playerAction of
                RightPaddleUp ->
                    ( { model | rightPaddleMovement = NotMoving }
                    , Cmd.none
                    )

                RightPaddleDown ->
                    ( { model | rightPaddleMovement = NotMoving }
                    , Cmd.none
                    )

                LeftPaddleUp ->
                    ( { model | leftPaddleMovement = NotMoving }
                    , Cmd.none
                    )

                LeftPaddleDown ->
                    ( { model | leftPaddleMovement = NotMoving }
                    , Cmd.none
                    )


updatePaddle : PaddleMovement -> Paddle -> Paddle
updatePaddle movement paddle =
    let
        amount =
            case movement of
                MovingUp ->
                    -10

                MovingDown ->
                    10

                NotMoving ->
                    0
    in
    case paddle of
        RightPaddle paddleInfo ->
            { paddleInfo
                | y =
                    (paddleInfo.y + amount)
                        |> clamp 0 (500 - paddleInfo.height)
            }
                |> RightPaddle

        LeftPaddle paddleInfo ->
            { paddleInfo
                | y =
                    (paddleInfo.y + amount)
                        |> clamp 0 (500 - paddleInfo.height)
            }
                |> LeftPaddle


shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball =
    case paddle of
        LeftPaddle { x, y, width, height } ->
            (ball.x - ball.radius <= x + width)
                && (ball.y >= y)
                && (ball.y <= y + height)

        RightPaddle { x, y, height } ->
            (ball.x + ball.radius >= x)
                && (ball.y >= y)
                && (ball.y <= y + height)


shouldBallBounceVertically : Ball -> Bool
shouldBallBounceVertically ball =
    let
        radius =
            ball.radius
    in
    ball.y <= radius || ball.y >= (500 - radius)


maybeWinner : Ball -> Maybe Player
maybeWinner ball =
    if ball.x <= ball.radius then
        Just RightPlayer

    else if ball.x >= (500 - ball.radius) then
        Just LeftPlayer

    else
        Nothing


view : Model -> Svg.Svg Msg
view { ball, rightPaddle, leftPaddle } =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        , viewPaddle rightPaddle
        , viewPaddle leftPaddle
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
    let
        paddleInfo =
            case paddle of
                LeftPaddle info ->
                    info

                RightPaddle info ->
                    info
    in
    rect
        [ x <| String.fromInt paddleInfo.x
        , y <| String.fromInt paddleInfo.y
        , width <| String.fromInt paddleInfo.width
        , height <| String.fromInt paddleInfo.height
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder PlayerAction
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction


keyToPlayerAction : String -> Decode.Decoder PlayerAction
keyToPlayerAction keyString =
    case keyString of
        "ArrowUp" ->
            Decode.succeed RightPaddleUp

        "ArrowDown" ->
            Decode.succeed RightPaddleDown

        "e" ->
            Decode.succeed LeftPaddleUp

        "d" ->
            Decode.succeed LeftPaddleDown

        _ ->
            Decode.fail "not an event we care about"
