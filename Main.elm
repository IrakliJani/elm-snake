module Main exposing (..)

import Browser
import Browser.Events
import Debug
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Random
import Result exposing (Result(..), andThen)
import Svg exposing (image, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, xlinkHref, y)
import Time


type alias Coord =
    ( Int, Int )


type alias Snake =
    List Coord


type Direction
    = Up
    | Down
    | Left
    | Right


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg s =
    case s of
        "ArrowLeft" ->
            Decode.succeed (KeyPress Left)

        "ArrowRight" ->
            Decode.succeed (KeyPress Right)

        "ArrowUp" ->
            Decode.succeed (KeyPress Up)

        "ArrowDown" ->
            Decode.succeed (KeyPress Down)

        _ ->
            Decode.fail ("Not interested in " ++ s)


type alias Model =
    { interval : Int
    , nextDirection : Direction
    , direction : Direction
    , snake : Snake
    , food : Maybe Coord
    }


type Msg
    = Tick Time.Posix
    | DecrementInterval Time.Posix
    | KeyPress Direction
    | SetFood Coord


type alias Update =
    ( Model, Cmd Msg )


config =
    { boxSize = 20
    , xBound = 40
    , yBound = 30
    , initialSnakeLength = 5
    , initialInterval = 100
    , lowestInterval = 50
    , decrementInterval = 20 * 1000
    , decrementIntervalBy = 5
    }


snakeHead : Snake -> Maybe Coord
snakeHead =
    List.Extra.last


snakeBody : Snake -> Maybe (List Coord)
snakeBody =
    List.Extra.init


any : List Bool -> Bool
any =
    List.foldl (||) False


randomCoord : Int -> Int -> Random.Generator Coord
randomCoord xBound yBound =
    Random.map2
        (\x y -> ( x, y ))
        (Random.int 0 (xBound - 1))
        (Random.int 0 (yBound - 1))


intersects : Coord -> Coord -> Bool
intersects a b =
    a == b



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { interval = config.initialInterval
      , nextDirection = Right
      , direction = Right
      , food = Nothing
      , snake = List.map (\x -> ( config.initialSnakeLength + x, config.initialSnakeLength )) (List.range 0 config.initialSnakeLength)
      }
    , Random.generate SetFood (randomCoord config.xBound config.yBound)
    )



-- UPDATE


setDirection : Update -> Result Update Update
setDirection ( model, cmd ) =
    Ok ( { model | direction = model.nextDirection }, cmd )


moveSnake : Update -> Result Update Update
moveSnake ( model, cmd ) =
    let
        updateDirection direction =
            case direction of
                Up ->
                    Tuple.mapSecond
                        (\y ->
                            if y - 1 < 0 then
                                config.yBound - 1

                            else
                                y - 1
                        )

                Left ->
                    Tuple.mapFirst
                        (\x ->
                            if x - 1 < 0 then
                                config.xBound - 1

                            else
                                x - 1
                        )

                Down ->
                    Tuple.mapSecond
                        (\y ->
                            if y + 1 >= config.yBound then
                                0

                            else
                                y + 1
                        )

                Right ->
                    Tuple.mapFirst
                        (\x ->
                            if x + 1 >= config.xBound then
                                0

                            else
                                x + 1
                        )

        move direction snake_ =
            case snakeHead snake_ of
                Just head ->
                    snake_ ++ [ updateDirection direction head ]

                Nothing ->
                    snake_
    in
    Ok ( { model | snake = move model.nextDirection model.snake }, cmd )


checkCollision : Update -> Result Update Update
checkCollision ( model, cmd ) =
    let
        body__ =
            snakeBody model.snake

        head__ =
            snakeHead model.snake
    in
    case ( body__, head__ ) of
        ( Just b, Just h ) ->
            if any (List.map (\part -> intersects part h) b) then
                Err init

            else
                Ok ( model, cmd )

        _ ->
            Ok ( model, cmd )


eatFood : Update -> Result Update Update
eatFood ( model, cmd ) =
    case ( model.food, snakeHead model.snake ) of
        ( Just food, Just head ) ->
            if intersects food head then
                Ok ( model, Random.generate SetFood (randomCoord config.xBound config.yBound) )

            else
                Ok ( { model | snake = List.drop 1 model.snake }, Cmd.none )

        _ ->
            Ok ( model, Cmd.none )


nextDirection : Direction -> Direction -> Direction
nextDirection prev next =
    case ( prev, next ) of
        ( Down, Up ) ->
            Down

        ( Up, Down ) ->
            Up

        ( Right, Left ) ->
            Right

        ( Left, Right ) ->
            Left

        _ ->
            next


update : Msg -> Model -> Update
update msg model =
    case msg of
        Tick _ ->
            case
                ( model, Cmd.none )
                    |> setDirection
                    |> andThen moveSnake
                    |> andThen checkCollision
                    |> andThen eatFood
            of
                Ok result ->
                    result

                Err result ->
                    result

        DecrementInterval _ ->
            ( { model
                | interval =
                    if model.interval == config.lowestInterval then
                        model.interval

                    else
                        model.interval - config.decrementIntervalBy
              }
            , Cmd.none
            )

        KeyPress direction ->
            ( { model | nextDirection = nextDirection model.direction direction }, Cmd.none )

        SetFood foodCoord ->
            ( { model | food = Just foodCoord }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { interval } =
    Sub.batch
        [ Time.every (toFloat interval) Tick
        , Time.every (toFloat config.decrementInterval) DecrementInterval
        , Browser.Events.onKeyDown keyDecoder
        ]



-- VIEWS


view : Model -> Html Msg
view model =
    let
        width_ =
            Debug.toString (config.xBound * config.boxSize)

        height_ =
            Debug.toString (config.yBound * config.boxSize)

        viewBox_ =
            "0 0 " ++ width_ ++ " " ++ height_

        backgroundView =
            rect [ width width_, height height_, fill "#F8F5F0" ] []

        snakeView =
            snake model.snake

        khinkaliView =
            case khinkali model.food of
                Just view_ ->
                    backgroundView :: view_ :: snakeView

                Nothing ->
                    backgroundView :: snakeView
    in
    div
        [ style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ div [ style "margin-bottom" "10px", style "font-family" "monospace", style "font-size" "20px", style "font-weight" "bold" ]
            [ text ("Score: " ++ Debug.toString ((List.length model.snake - config.initialSnakeLength - 1) * 10))
            ]
        , svg
            [ width width_
            , height height_
            , viewBox viewBox_
            ]
            khinkaliView
        ]


snake : Snake -> List (Html Msg)
snake =
    let
        body ( x1, y2 ) =
            rect
                [ x (Debug.toString (x1 * config.boxSize))
                , y (Debug.toString (y2 * config.boxSize))
                , width (Debug.toString config.boxSize)
                , height (Debug.toString config.boxSize)
                , fill "#C54D48"
                , stroke "white"
                , Svg.Attributes.strokeWidth "1"
                ]
                []
    in
    List.map body


khinkali : Maybe Coord -> Maybe (Html Msg)
khinkali coord =
    let
        imageMapper ( x_, y_ ) =
            image
                [ x (Debug.toString (x_ * config.boxSize))
                , y (Debug.toString (y_ * config.boxSize))
                , width (Debug.toString config.boxSize)
                , height (Debug.toString config.boxSize)
                , xlinkHref "khinkali.jpg"
                ]
                []
    in
    Maybe.map imageMapper coord



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
