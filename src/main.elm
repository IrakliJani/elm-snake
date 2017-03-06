import Html exposing (Html)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke, xlinkHref)
import Time exposing (Time, millisecond, second)
import Keyboard exposing (KeyCode)
import Random

import Shared exposing (..)
import Controls exposing (Direction (..), mapKeyCode, nextDirection, moveSnake, eatFood, intersects)

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

initialSnake : Snake
initialSnake =
  (5, 5)
  |> List.repeat 5
  |> List.indexedMap (\index -> Tuple.mapFirst (\x -> x + index))

randomFoodCoord : Random.Generator Coord
randomFoodCoord = (Random.map2 (,) (Random.int 0 (xBound - 1)) (Random.int 0 (yBound - 1)))

init : (Model, Cmd Msg)
init =
  ( { interval = initialInterval
    , direction = Right
    , lastDirection = Right
    , snake = initialSnake
    , food = Nothing
    }
  , Random.generate SetFood randomFoodCoord
  )

-- UPDATE

type Msg
  = Tick Time
  | IncrementInterval Time
  | KeyPress KeyCode
  | SetFood Coord

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      let
        nextSnake = moveSnake model.direction model.snake
        vograshiSnake = eatFood model.food nextSnake
      in
        ({ model | snake = vograshiSnake
                 , lastDirection = model.direction}
        , case Maybe.map2 (,) model.food (snakeHead nextSnake) of
            Just (food, head) ->
              if intersects head food
                then Random.generate SetFood randomFoodCoord
                else Cmd.none
            _ -> Cmd.none
        )

    IncrementInterval _ ->
      ({ model | interval = model.interval - 10 }, Cmd.none)

    KeyPress keyCode ->
      case mapKeyCode keyCode of
        Just direction -> ({ model | direction = nextDirection model.lastDirection direction }, Cmd.none)
        Nothing -> (model, Cmd.none)

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
