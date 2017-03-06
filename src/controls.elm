module Controls exposing
  ( Direction (..)
  , mapKeyCode
  , nextDirection
  , moveSnake
  , eatFood
  , intersects
  )

import Shared exposing (..)
import Keyboard exposing (KeyCode)

type Direction
  = Up
  | Down
  | Left
  | Right

mapKeyCode : KeyCode -> Maybe Direction
mapKeyCode keyCode =
  case keyCode of
    37 -> Just Left
    38 -> Just Up
    39 -> Just Right
    40 -> Just Down
    _ -> Nothing

nextDirection : Direction -> Direction -> Direction
nextDirection prev next =
  case next of
    Up -> if prev == Down then Down else Up
    Down -> if prev == Up then Up else Down
    Left -> if prev == Right then Right else Left
    Right -> if prev == Left then Left else Right

updateDirection : Direction -> Coord -> Coord
updateDirection direction =
  case direction of
    Up ->    Tuple.mapSecond (\y -> if y - 1 < 0 then yBound - 1 else y - 1)
    Left ->  Tuple.mapFirst  (\x -> if x - 1 < 0 then xBound - 1 else x - 1)
    Down ->  Tuple.mapSecond (\y -> if y + 1 >= yBound then 0 else y + 1)
    Right -> Tuple.mapFirst  (\x -> if x + 1 >= xBound then 0 else x + 1)

moveSnake : Direction -> Snake -> Snake
moveSnake direction snake =
  case snakeHead snake of
    Just head -> snake ++ [updateDirection direction head]
    Nothing -> snake

eatFood : Maybe Coord -> Snake -> Snake
eatFood food snake =
  case Maybe.map2 (,) food (snakeHead snake) of
    Just (food, head) -> if intersects head food then snake else List.drop 1 snake
    _ -> snake


intersects : Coord -> Coord -> Bool
intersects a b = a == b
