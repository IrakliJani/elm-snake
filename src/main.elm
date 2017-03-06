import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke)
import Time exposing (Time, millisecond, second)
import Keyboard exposing (KeyCode)

last : List a -> Maybe a
last list = list |> List.reverse |> List.head

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type Direction
  = Up
  | Down
  | Left
  | Right

type alias Model =
  { interval : Time
  , direction : Direction
  , snake : List (Int, Int)
  }

snakeSize : Int
snakeSize = 10

initialInterval : Time
initialInterval = 200

initialSnake : List (Int, Int)
initialSnake =
  let
    startPosition = snakeSize * 5
    startCoords = (startPosition, startPosition)
    coordsMapper = (\index -> Tuple.mapFirst (\x -> x + snakeSize * index))
  in
    startCoords
    |> List.repeat 5
    |> List.indexedMap coordsMapper

init : (Model, Cmd Msg)
init =
  ( { interval = initialInterval
    , direction = Right
    , snake = initialSnake
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = Tick Time
  | IncrementInterval Time
  | KeyPress KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    updateDirection =
      case model.direction of
        Up ->    Tuple.mapSecond (\y -> y - snakeSize)
        Down ->  Tuple.mapSecond (\y -> y + snakeSize)
        Left ->  Tuple.mapFirst  (\x -> x - snakeSize)
        Right -> Tuple.mapFirst  (\x -> x + snakeSize)

    move snake =
      case last snake of
        Just head -> List.drop 1 snake ++ [updateDirection head]
        Nothing -> snake

  in
    case msg of
      Tick _ ->
        ({ model | snake = (move model.snake) }, Cmd.none)

      IncrementInterval _ ->
        ({ model | interval = model.interval - 10 }, Cmd.none)

      KeyPress keyCode ->
        case keyCode of
          37 -> ({ model | direction = if model.direction == Right then Right else Left }, Cmd.none)
          38 -> ({ model | direction = if model.direction == Down then Down else Up }, Cmd.none)
          39 -> ({ model | direction = if model.direction == Left then Left else Right }, Cmd.none)
          40 -> ({ model | direction = if model.direction == Up then Up else Down }, Cmd.none)
          _ -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions {interval} =
  Sub.batch
    [ Time.every (millisecond * interval) Tick
    , Time.every (second * 30) IncrementInterval
    , Keyboard.downs KeyPress
    ]

-- VIEW

view : Model -> Html Msg
view model =
  let
    svgAttributes = [ width "800", height "600", viewBox "0 0 800 600" ]
    background = rect [ width "800", height "600", fill "#EFEFEF" ] []
    snake = snakeView model
  in
    svg svgAttributes (background :: snake)

snakeView : Model -> List (Html Msg)
snakeView {snake} =
  List.map (\s -> body (Tuple.first s) (Tuple.second s)) snake

body : Int -> Int -> Html Msg
body x1 y2 =
  rect
    [ x (toString x1)
    , y (toString y2)
    , width (toString snakeSize)
    , height (toString snakeSize)
    , fill "black"
    , stroke "white"
    ] []
