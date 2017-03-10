module Controls exposing
  ( Direction (..)
  , mapKeyCode
  , nextDirection
  )

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
      Up ->
        if prev == Down then Down else Up

      Down ->
        if prev == Up then Up else Down

      Left ->
        if prev == Right then Right else Left

      Right ->
        if prev == Left then Left else Right
