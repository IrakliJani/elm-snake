import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke, xlinkHref)
import Time exposing (Time, millisecond, second)
import Keyboard exposing (KeyCode)
import Random
import Result exposing (Result (..), andThen)
import List.Extra


type alias Coord =
    (Int, Int)


type alias Snake =
    List Coord


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { interval : Time
    , nextDirection : Direction
    , direction : Direction
    , snake : Snake
    , food : Maybe Coord
    }


type Msg
    = Tick Time
    | DecrementInterval Time
    | KeyPress KeyCode
    | SetFood Coord


type alias Update =
    (Model, Cmd Msg)


config =
    { boxSize = 20
    , xBound = 40
    , yBound = 30
    , initialInterval = 150
    , lowestInterval = 50
    , decrementInterval = 20
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
    Random.map2 (,) (Random.int 0 (xBound - 1)) (Random.int 0 (yBound - 1))


intersects : Coord -> Coord -> Bool
intersects a b =
    a == b


keyCodeToDirection : KeyCode -> Maybe Direction
keyCodeToDirection keyCode =
    case keyCode of
        37 -> Just Left
        38 -> Just Up
        39 -> Just Right
        40 -> Just Down
        _ -> Nothing



-- MAIN



main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL



init : Update
init =
    ( { interval = config.initialInterval
      , nextDirection = Right
      , direction = Right
      , food = Nothing
      , snake = List.map (\x-> (5 + x, 5)) (List.range 0 5)
      }
    , Random.generate SetFood (randomCoord config.xBound config.yBound)
    )



-- UPDATE



setDirection : Update -> Result Update Update
setDirection (model, cmd) =
    Ok ({ model | direction = model.nextDirection }, cmd)


moveSnake : Update -> Result Update Update
moveSnake (model, cmd) =
    let
        updateDirection direction =
            case direction of
                Up ->
                    Tuple.mapSecond (\y -> if y - 1 < 0 then config.yBound - 1 else y - 1)

                Left ->
                    Tuple.mapFirst  (\x -> if x - 1 < 0 then config.xBound - 1 else x - 1)

                Down ->
                    Tuple.mapSecond (\y -> if y + 1 >= config.yBound then 0 else y + 1)

                Right ->
                    Tuple.mapFirst  (\x -> if x + 1 >= config.xBound then 0 else x + 1)

        move direction snake =
            case snakeHead snake of
                Just head ->
                    snake ++ [updateDirection direction head]

                Nothing ->
                    snake
    in
        Ok ({ model | snake = move model.nextDirection model.snake }, cmd)


checkCollision : Update -> Result Update Update
checkCollision (model, cmd) =
    let
        body = snakeBody model.snake
        head = snakeHead model.snake
    in
        case (body, head) of
            (Just body, Just head) ->
                if any (List.map (\part -> intersects part head) body)
                then
                    Err init
                else
                    Ok (model, cmd)
            _ ->
                Ok (model, cmd)


eatFood : Update -> Result Update Update
eatFood (model, cmd) =
    let
        snake = model.snake
        food = model.food
        head = snakeHead snake
    in
        case (food, head) of
            (Just food, Just head) ->
                if intersects food head
                then
                    Ok (model, Random.generate SetFood (randomCoord config.xBound config.yBound))
                else
                    Ok ({ model | snake = List.drop 1 snake }, Cmd.none)
            _ ->
                Ok (model, Cmd.none)


nextDirection : Direction -> Direction -> Direction
nextDirection prev next =
    case (prev, next) of
        (Down, Up) -> Down
        (Up, Down) -> Up
        (Right, Left) -> Right
        (Left, Right) ->  Left
        _ -> next


update : Msg -> Model -> Update
update msg model =
    case msg of
        Tick _ ->
            case (model, Cmd.none)
                |> setDirection
                |> andThen moveSnake
                |> andThen checkCollision
                |> andThen eatFood
            of
                Ok result -> result
                Err result -> result

        DecrementInterval _ ->
            ({ model | interval = if model.interval == config.lowestInterval
                                  then model.interval
                                  else model.interval - 10 }
            , Cmd.none)

        KeyPress keyCode ->
            case keyCodeToDirection keyCode of
                Just direction ->
                    ({ model | nextDirection = nextDirection model.direction direction }, Cmd.none)

                Nothing ->
                    (model, Cmd.none)

        SetFood foodCoord ->
            ({ model | food = Just foodCoord }, Cmd.none)



-- SUBSCRIPTIONS



subscriptions : Model -> Sub Msg
subscriptions {interval} =
    Sub.batch
        [ Time.every (millisecond * interval) Tick
        , Time.every (second * config.decrementInterval) DecrementInterval
        , Keyboard.downs KeyPress
        ]



-- VIEWS



view : Model -> Html Msg
view model =
    let
        width_ = toString (config.xBound * config.boxSize)
        height_ = toString (config.yBound * config.boxSize)
        viewBox_ = "0 0 " ++ width_ ++ " " ++ height_

        backgroundView = rect [ width width_, height height_, fill "#F8F5F0" ] []
        snakeView = snake model.snake

        view =
            case khinkali model.food of
                Just khinkaliView ->
                    backgroundView :: khinkaliView :: snakeView

                Nothing ->
                    backgroundView :: snakeView

    in
        div [ style [ ("height", "100vh")
                    , ("display", "flex")
                    , ("align-items", "center")
                    , ("justify-content", "center")
                    ]
            ] [ svg [ width width_
                    , height height_
                    , viewBox viewBox_
                    ] view
              ]


snake : Snake -> List (Html Msg)
snake =
    let
        body (x1, y2) =
            rect
                [ x (toString (x1 * config.boxSize))
                , y (toString (y2 * config.boxSize))
                , width (toString config.boxSize)
                , height (toString config.boxSize)
                , fill "#C54D48"
                , stroke "white"
                , Svg.Attributes.strokeWidth "1"
                ] []
    in
        List.map body


khinkali : Maybe Coord -> Maybe (Html Msg)
khinkali coord =
    let
        imageMapper (x_, y_) =
            image [ x (toString (x_ * config.boxSize))
                  , y (toString (y_ * config.boxSize))
                  , width (toString config.boxSize)
                  , height (toString config.boxSize)
                  , xlinkHref "/assets/khinkali.jpg"
                  ] []
    in
        Maybe.map imageMapper coord
