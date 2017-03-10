module Coordinator exposing
  ( randomCoord
  , updateDirection
  , moveSnake
  , eatFood
  )

import Random
import Shared exposing (..)
import Controls exposing (Direction (..))


randomCoord : Int -> Int -> Random.Generator Coord
randomCoord xBound yBound =
    Random.map2 (,) (Random.int 0 (xBound - 1)) (Random.int 0 (yBound - 1))


updateDirection : Direction -> Coord -> Coord
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


moveSnake : Direction -> Snake -> Snake
moveSnake direction snake =
    case snakeHead snake of
        Just head ->
            snake ++ [updateDirection direction head]

        Nothing ->
            snake


eatFood : Maybe Coord -> Snake -> Snake
eatFood food snake =
    if intersects food (snakeHead snake)
    then snake
    else List.drop 1 snake
