module Shared exposing (..)

type alias Coord = (Int, Int)
type alias Snake = List Coord

boxSize = 20
xBound = 40
yBound = 30
initialInterval = 100

snakeHead snake = snake |> List.reverse |> List.head
