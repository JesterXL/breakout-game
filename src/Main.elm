module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyPress, onKeyUp, onVisibilityChange)
import Canvas exposing (arc, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Json.Decode as Decode
import Random


type alias Model =
    { paused : Bool
    , gravity : Float
    , frame : Int
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , screenHidden : Bool
    , gameOver : Bool
    , myBall : Component
    , myPaddle : Component
    , dx : Float
    , dy : Float
    , rightPressed : Bool
    , leftPressed : Bool
    }


initialModel : Random.Seed -> Model
initialModel seed =
    { paused = False
    , gravity = 0.05
    , frame = 0
    , initialSeed = seed
    , currentSeed = seed
    , screenHidden = False
    , gameOver = False
    , myBall =
        { type_ = "ball"
        , score = 0
        , width = 20
        , height = 20
        , speedX = 0
        , speedY = 0
        , x = canvasWidth / 2
        , y = canvasHeight - 30
        , gravity = 0.05
        , gravitySpeed = 0
        , radius = 10.0
        }
    , myPaddle =
        { type_ = "paddle"
        , score = 0
        , width = 75
        , height = 10
        , speedX = 0
        , speedY = 0
        , x = (canvasWidth - 75) / 2
        , y = canvasHeight - 10
        , gravity = 0.05
        , gravitySpeed = 0
        , radius = 0
        }
    , dx = 2
    , dy = -2
    , rightPressed = False
    , leftPressed = False
    }


type alias Component =
    { type_ : String
    , score : Int
    , width : Float
    , height : Float
    , speedX : Float
    , speedY : Float
    , x : Float
    , y : Float
    , gravity : Float
    , gravitySpeed : Float
    , radius : Float
    }


type Msg
    = TogglePause -- pause and unpause the game
    | Frame Float -- the browser ticks; sends this every time it's ready to update an animation frame
    | Accelerate Float -- accelerate the square up or down
    | VisibilityChange Visibility -- when the browser tab is hidden or shown
    | DirectionChanged Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Frame timePassed ->
            let
                updatedframe =
                    model.frame + 1

                updatedDX =
                    updateDirectionX model.myBall model.dx model.myBall.radius

                updatedDY =
                    updateDirectionY model.myBall model.dy model.myBall.radius model.myPaddle.x model.myPaddle.width

                isGameOver =
                    updateGameOver model.myBall updatedDY
            in
            if isGameOver == False then
                ( { model
                    | frame = updatedframe
                    , myBall = updateBall model.myBall timePassed updatedframe updatedDX updatedDY
                    , myPaddle = updatePaddle model.myPaddle model.rightPressed model.leftPressed
                    , dx = updatedDX
                    , dy = updatedDY
                  }
                , Cmd.none
                )

            else
                ( { model
                    | frame = updatedframe
                    , gameOver = True
                  }
                , Cmd.none
                )

        Accelerate amount ->
            ( model, Cmd.none )

        VisibilityChange vis ->
            case vis of
                Hidden ->
                    ( { model | paused = True, screenHidden = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False, screenHidden = False }, Cmd.none )

        DirectionChanged dir ->
            case dir of
                LeftPressed ->
                    ( { model | leftPressed = True }, Cmd.none )

                RightPressed ->
                    ( { model | rightPressed = True }, Cmd.none )

                LeftReleased ->
                    ( { model | leftPressed = False }, Cmd.none )

                RightReleased ->
                    ( { model | rightPressed = False }, Cmd.none )

                Other ->
                    ( model, Cmd.none )


updateBall : Component -> Float -> Int -> Float -> Float -> Component
updateBall ball timePassed updatedframe dx dy =
    { ball | x = ball.x + dx, y = ball.y + dy }


updateDirectionX : Component -> Float -> Float -> Float
updateDirectionX ball dx ballRadius =
    if ball.x + dx > canvasWidth - ballRadius || ball.x + dx < ballRadius then
        dx * -1

    else
        dx


updateDirectionY : Component -> Float -> Float -> Float -> Float -> Float
updateDirectionY ball dy ballRadius paddleX paddleWidth =
    if ball.y + dy > canvasHeight - ballRadius then
        if ball.x > paddleX && ball.x < paddleX + paddleWidth then
            dy * -1

        else
            dy

    else if ball.y + dy < ballRadius then
        dy * -1

    else
        dy


updatePaddle : Component -> Bool -> Bool -> Component
updatePaddle myPaddle rightPressed leftPressed =
    if rightPressed == True then
        { myPaddle | x = min (myPaddle.x + 7) (canvasWidth - myPaddle.width) }

    else if leftPressed == True then
        { myPaddle | x = max (myPaddle.x - 7) 0 }

    else
        myPaddle


updateGameOver : Component -> Float -> Bool
updateGameOver ball dy =
    if ball.y + dy > canvasHeight - ball.radius then
        True

    else
        False


hitPaddle : Component -> Float -> Float -> Float -> Bool
hitPaddle ball dy paddleX paddleWidth =
    if ball.y + dy > canvasHeight - ball.radius then
        if ball.x > paddleX && ball.x < paddleX + paddleWidth then
            True

        else
            False

    else
        False


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "padding-bottom" "8px"
            ]
            [ Html.text ("Score:" ++ String.fromInt model.frame) ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ Canvas.toHtml
                ( round canvasWidth, round canvasHeight )
                [ style "border" "1px solid rgba(0,0,0,1)" ]
                (clearScreen
                    :: drawMyBall model.myBall
                    :: [ drawMyPaddle model.myPaddle ]
                )
            ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "padding-top" "8px"
            ]
            (viewGameButtons model)
        ]


viewGameButtons : Model -> List (Html Msg)
viewGameButtons model =
    if model.gameOver == False then
        [ if model.paused == True then
            button [ onClick TogglePause ] [ Html.text "Unpause" ]

          else
            button [ onClick TogglePause ] [ Html.text "Pause" ]
        , if model.leftPressed == True then
            button [] [ Html.text "Left Pressed" ]

          else
            button [] [ Html.text "Left" ]
        , if model.rightPressed == True then
            button [] [ Html.text "Right Pressed" ]

          else
            button [] [ Html.text "Right" ]
        ]

    else
        [ div [] [ Html.text "Game Over" ] ]


drawMyBall : Component -> Canvas.Renderable
drawMyBall component =
    shapes
        [ fill (Color.hsl 0.5 0.3 0.7) ]
        [ arc ( component.x, component.y ) component.radius { startAngle = degrees 10, endAngle = degrees 0, clockwise = True } ]


drawMyPaddle : Component -> Canvas.Renderable
drawMyPaddle component =
    shapes
        [ fill (Color.hsl 0.5 0.3 0.7) ]
        [ rect ( component.x, component.y ) component.width component.height ]


canvasWidth : Float
canvasWidth =
    480


canvasHeight : Float
canvasHeight =
    270


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]



-- For now, we pass in Math.random() from JavaScript. At some point,
-- we should get a better random number generator.


init : Int -> ( Model, Cmd Msg )
init datRando =
    ( initialModel (Random.initialSeed datRando), Cmd.none )



-- if paused, stop sending animation frame updates


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameOver == True then
        Sub.none

    else if model.paused == False then
        Sub.batch
            [ onVisibilityChange VisibilityChange
            , onAnimationFrameDelta Frame
            , onKeyDown keyDecoderPressed
            , onKeyUp keyDecoderReleased
            ]

    else
        onVisibilityChange VisibilityChange


type Direction
    = LeftPressed
    | RightPressed
    | LeftReleased
    | RightReleased
    | Other


keyDecoderPressed : Decode.Decoder Msg
keyDecoderPressed =
    Decode.map toDirectionPressed (Decode.field "key" Decode.string)


toDirectionPressed : String -> Msg
toDirectionPressed string =
    case string of
        "ArrowLeft" ->
            DirectionChanged LeftPressed

        "ArrowRight" ->
            DirectionChanged RightPressed

        _ ->
            DirectionChanged Other


keyDecoderReleased : Decode.Decoder Msg
keyDecoderReleased =
    Decode.map toDirectionReleased (Decode.field "key" Decode.string)


toDirectionReleased : String -> Msg
toDirectionReleased string =
    case string of
        "ArrowLeft" ->
            DirectionChanged LeftReleased

        "ArrowRight" ->
            DirectionChanged RightReleased

        _ ->
            DirectionChanged Other


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
