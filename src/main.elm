import Html exposing (Html)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke, xlinkHref)
import Time exposing (Time, millisecond, second)
import Keyboard exposing (KeyCode)
import Random
import Result exposing (Result (..), andThen)
import Shared exposing (..)
import Controls exposing (Direction (..), mapKeyCode, nextDirection)
import Coordinator exposing (randomCoord, moveSnake, eatFood)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL



type alias Model =
    { interval : Time
    , nextDirection : Direction
    , direction : Direction
    , snake : Snake
    , food : Maybe Coord
    }


init : Update
init =
    ( { interval = 100
      , nextDirection = Right
      , direction = Right
      , snake = (5, 5) |> List.repeat 5 |> List.indexedMap (\index -> Tuple.mapFirst (\x -> x + index))
      , food = Nothing
      }
    , Random.generate SetFood (randomCoord config.xBound config.yBound)
    )



-- UPDATE



type Msg
    = Tick Time
    | IncrementInterval Time
    | KeyPress KeyCode
    | SetFood Coord


type alias Update =
    (Model, Cmd Msg)


setDirection : Update -> Result Update Update
setDirection (model, cmd) =
    Ok ({ model | direction = model.nextDirection }, cmd)


moveSnake_ : Update -> Result Update Update
moveSnake_ (model, cmd) =
    Ok ({ model | snake = moveSnake model.nextDirection model.snake }, cmd)


checkCollision : Update -> Result Update Update
checkCollision (model, cmd) =
    case Maybe.map2 (,) (snakeBody model.snake) (snakeHead model.snake) of
        Just (body, head) ->
            if any (List.map (\part -> intersects (Just part) (Just head)) body)
            then Err init
            else Ok (model, cmd)
        _ -> Ok (model, cmd)


eatFood_ : Update -> Result Update Update
eatFood_ (model, cmd) =
    Ok ( { model | snake = eatFood model.food model.snake }
       , if intersects model.food (snakeHead model.snake)
        then Random.generate SetFood (randomCoord config.xBound config.yBound)
        else Cmd.none
    )


update : Msg -> Model -> Update
update msg model =
    case msg of
        Tick _ ->
            case (model, Cmd.none)
                |> setDirection
                |> andThen moveSnake_
                |> andThen checkCollision
                |> andThen eatFood_
            of
                Ok result -> result
                Err result -> result

        IncrementInterval _ ->
            ({ model | interval = model.interval - 10 }, Cmd.none)

        KeyPress keyCode ->
            case mapKeyCode keyCode of
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
        , Time.every (second * 30) IncrementInterval
        , Keyboard.downs KeyPress
        ]



-- VIEWS



view : Model -> Html Msg
view model =
    let
        w = toString (config.xBound * config.boxSize)
        h = toString (config.yBound * config.boxSize)
        vb = "0 0 " ++ w ++ " " ++ h
        backgroundView = rect [ width w, height h, fill "#EFEFEF" ] []
        khinkaliView = khinkali model.food
        snakeView = snake model.snake
    in
        svg [ width w
            , height h
            , viewBox vb
            ]
            (case khinkaliView of
                Just khinkaliView ->
                    (backgroundView :: khinkaliView :: snakeView)
                Nothing ->
                    (backgroundView :: snakeView))


snake : Snake -> List (Html Msg)
snake =
    let
        body (x1, y2) =
            rect
                [ x (toString (x1 * config.boxSize))
                , y (toString (y2 * config.boxSize))
                , width (toString config.boxSize)
                , height (toString config.boxSize)
                , fill "black"
                , stroke "white"
                , Svg.Attributes.strokeWidth "2"
                ] []
    in
        List.map body


khinkali : Maybe Coord -> Maybe (Html Msg)
khinkali coord =
    case coord of
        Just (x1, y2) ->
            Just (image [ x (toString (x1 * config.boxSize))
                        , y (toString (y2 * config.boxSize))
                        , width (toString config.boxSize)
                        , height (toString config.boxSize)
                        , xlinkHref "/assets/images/khinkali@2x.png"
                        ]
                        [])
        Nothing ->
            Nothing
