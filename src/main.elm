import Html exposing (Html)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke, xlinkHref)
import Time exposing (Time, millisecond, second)
import Keyboard exposing (KeyCode)
import Random
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
  , direction : Direction
  , lastDirection : Direction
  , snake : Snake
  , food : Maybe Coord
  }

init : (Model, Cmd Msg)
init =
  ( { interval = 100
    , direction = Right
    , lastDirection = Right
    , snake = (5, 5) |> List.repeat 5 |> List.indexedMap (\index -> Tuple.mapFirst (\x -> x + index))
    , food = Nothing
    }
  , Random.generate SetFood (randomCoord xBound yBound)
  )

-- UPDATE

type Msg
  = Tick Time
  | IncrementInterval Time
  | KeyPress KeyCode
  | SetFood Coord

setDirection : (Model, Cmd Msg) -> (Model, Cmd Msg)
setDirection (model, cmd) = ({ model | lastDirection = model.direction }, cmd)

moveSnake_ : (Model, Cmd Msg) -> (Model, Cmd Msg)
moveSnake_ (model, cmd) = ({ model | snake = moveSnake model.direction model.snake }, cmd)

checkCollision : (Model, Cmd Msg) -> (Model, Cmd Msg)
checkCollision (model, cmd) =
  case Maybe.map2 (,) (snakeBody model.snake) (snakeHead model.snake) of
    Just (body, head) ->
      if any (List.map (\part -> intersects (Just part) (Just head)) body)
      then init
      else (model, cmd)
    _ -> (model, cmd)

eatFood_ : (Model, Cmd Msg) -> (Model, Cmd Msg)
eatFood_ (model, cmd) =
  let
    newCmd = if intersects model.food (snakeHead model.snake)
          then Random.generate SetFood (randomCoord xBound yBound)
          else Cmd.none
  in
    if cmd == Cmd.none
    then ({ model | snake = eatFood model.food model.snake }, newCmd)
    else (model, cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      (model, Cmd.none) |> setDirection |> moveSnake_ |> checkCollision |> eatFood_

    IncrementInterval _ ->
      ({ model | interval = model.interval - 10 }, Cmd.none)

    KeyPress keyCode ->
      case mapKeyCode keyCode of
        Just direction ->
          ({ model | direction = nextDirection model.lastDirection direction }, Cmd.none)
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
    w = toString (xBound * boxSize)
    h = toString (yBound * boxSize)
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
          Just khinkaliView -> (backgroundView :: khinkaliView :: snakeView)
          Nothing -> (backgroundView :: snakeView))

snake : Snake -> List (Html Msg)
snake =
  let
    body (x1, y2) =
      rect
        [ x (toString (x1 * boxSize))
        , y (toString (y2 * boxSize))
        , width (toString boxSize)
        , height (toString boxSize)
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
      Just (image [ x (toString (x1 * boxSize))
                  , y (toString (y2 * boxSize))
                  , width (toString boxSize)
                  , height (toString boxSize)
                  , xlinkHref "/assets/images/khinkali@2x.png"
                  ]
                  [])
    Nothing -> Nothing
