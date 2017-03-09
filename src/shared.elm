module Shared exposing (..)

import List.Extra

type alias Coord = (Int, Int)
type alias Snake = List Coord

boxSize = 20
xBound = 40
yBound = 30

snakeHead : Snake -> Maybe Coord
snakeHead = List.Extra.last

snakeBody : Snake -> Maybe (List Coord)
snakeBody = List.Extra.init

any : List Bool -> Bool
any list =
  case list of
    [] -> False
    [x] -> x
    x::xs -> if x == True then True else any xs

intersects : Maybe Coord -> Maybe Coord -> Bool
intersects a b =
  case Maybe.map2 (,) a b of
    Just (a, b) -> a == b
    _ -> False
