module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


type alias Model =
    { ball : Ball
    , rightPaddle : Paddle
    , leftPaddle : Paddle
    , rightPaddleMovement : PaddleMovement
    , leftPaddleMovement : PaddleMovement
    , gameStatus : GameStatus
    , score : Score
    }


type alias Ball =
    { x : Int
    , y : Int
    , size : Int
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
    | RestartGame
    | NewWinner Player


type PlayerAction
    = RightPaddleUp
    | RightPaddleDown
    | LeftPaddleUp
    | LeftPaddleDown


type alias Score =
    { rightPlayerScore : Int
    , leftPlayerScore : Int
    }


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
      , score =
            { rightPlayerScore = 0
            , leftPlayerScore = 0
            }
      }
    , Cmd.none
    )


initBall : Ball
initBall =
    { x = 250
    , y = 250
    , size = 10
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
                updatedBall =
                    updateBall model

                updatedModel =
                    { model
                        | ball = updatedBall
                        , rightPaddle = updatePaddle model.rightPaddleMovement model.rightPaddle
                        , leftPaddle = updatePaddle model.leftPaddleMovement model.leftPaddle
                    }
            in
            case ( maybeWinner updatedBall, model.gameStatus ) of
                ( Just player, NoWinner ) ->
                    update (NewWinner player) updatedModel

                _ ->
                    ( updatedModel, Cmd.none )

        NewWinner player ->
            let
                alwaysRestartGame : a -> Msg
                alwaysRestartGame =
                    always RestartGame

                updatedScore =
                    updateScores model.score player

                sleepCmd =
                    Process.sleep 500
                        |> Task.perform alwaysRestartGame

                ( randomDirection, _ ) =
                    Random.initialSeed 42
                        |> Random.step (Random.int 0 100)
                        |> Debug.log "Random direction: "
            in
            ( { model | gameStatus = Winner player, score = updatedScore }
            , sleepCmd
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

        RestartGame ->
            ( { model
                | ball = initBall
                , gameStatus = NoWinner
              }
            , Cmd.none
            )


updateBall :
    { a
        | gameStatus : GameStatus
        , ball : Ball
        , rightPaddle : Paddle
        , leftPaddle : Paddle
    }
    -> Ball
updateBall { gameStatus, ball, rightPaddle, leftPaddle } =
    let
        maybeRightDistance =
            maybeBounceDistanceFromCenter rightPaddle ball

        maybeLeftDistance =
            maybeBounceDistanceFromCenter leftPaddle ball

        maybeDistance =
            -- Combine the two maybes and keep the one that isn't Nothing, if any.
            if maybeRightDistance == Nothing then
                maybeLeftDistance

            else
                maybeRightDistance

        ( horizSpeed, bouncedVertSpeed ) =
            case maybeDistance of
                Nothing ->
                    -- No bounce
                    ( ball.horizSpeed, ball.vertSpeed )

                Just distance ->
                    ( ball.horizSpeed * -1
                    , distance // 10
                    )

        shouldBounceVertically =
            shouldBallBounceVertically ball

        vertSpeed =
            if shouldBounceVertically then
                bouncedVertSpeed * -1

            else
                bouncedVertSpeed
    in
    case gameStatus of
        Winner _ ->
            ball

        NoWinner ->
            { ball
                | x = ball.x + horizSpeed
                , y = ball.y + vertSpeed
                , horizSpeed = horizSpeed
                , vertSpeed = vertSpeed
            }


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


maybeBounceDistanceFromCenter : Paddle -> Ball -> Maybe Int
maybeBounceDistanceFromCenter paddle ball =
    -- If the ball bounces, return Just the distance from the paddle center in
    -- percentage, so -100% if it's the very top of the paddle, 100% if it's
    -- the very bottom of the paddle.
    let
        normalize : Int -> Int -> Int
        normalize distance height =
            (distance - (height // 2)) * 100 // (height // 2)
    in
    case paddle of
        LeftPaddle { x, y, width, height } ->
            if
                (ball.x <= x + width)
                    && (ball.y >= y)
                    && (ball.y <= y + height)
                    && (ball.horizSpeed < 0)
            then
                Just <| normalize (ball.y - y) height

            else
                Nothing

        RightPaddle { x, y, height } ->
            if
                (ball.x + ball.size >= x)
                    && (ball.y >= y)
                    && (ball.y <= y + height)
                    && (ball.horizSpeed > 0)
            then
                Just <| normalize (ball.y - y) height

            else
                Nothing


shouldBallBounceVertically : Ball -> Bool
shouldBallBounceVertically ball =
    let
        size =
            ball.size
    in
    ball.y <= size || ball.y >= (500 - size)


maybeWinner : Ball -> Maybe Player
maybeWinner ball =
    if ball.x <= ball.size then
        Just RightPlayer

    else if ball.x >= (500 - ball.size) then
        Just LeftPlayer

    else
        Nothing


updateScores : Score -> Player -> Score
updateScores score winner =
    case winner of
        RightPlayer ->
            { score | rightPlayerScore = score.rightPlayerScore + 1 }

        LeftPlayer ->
            { score | leftPlayerScore = score.leftPlayerScore + 1 }


view : Model -> Svg.Svg Msg
view { ball, rightPaddle, leftPaddle, score } =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewDivider
        , viewBall ball
        , viewPaddle rightPaddle
        , viewPaddle leftPaddle
        , viewScore score
        ]


viewDivider : Svg.Svg Msg
viewDivider =
    line
        [ x1 "249"
        , y1 "0"
        , x2 "249"
        , y2 "500"
        , stroke "black"
        , strokeDasharray "4"
        , strokeWidth "2"
        ]
        []


viewBall : Ball -> Svg.Svg Msg
viewBall ball =
    rect
        [ x <| String.fromInt ball.x
        , y <| String.fromInt ball.y
        , width <| String.fromInt ball.size
        , height <| String.fromInt ball.size
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


viewScore : Score -> Svg.Svg Msg
viewScore score =
    g
        [ fontSize "100px"
        , fontFamily "monospace"
        ]
        [ text_ [ x "50", y "100", textAnchor "start" ]
            [ text <| String.fromInt score.leftPlayerScore ]
        , text_ [ x "450", y "100", textAnchor "end" ]
            [ text <| String.fromInt score.rightPlayerScore ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
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
