-- https://exercism.org/tracks/haskell/exercises/resistor-color-duo/

module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

value' :: Color -> Int
value' a = case a of 
            Black -> 0
            Brown -> 1
            Red -> 2
            Orange -> 3
            Yellow -> 4
            Green -> 5
            Blue -> 6
            Violet -> 7
            Grey -> 8
            White -> 9


value :: (Color, Color) -> Int
value (a, b) = 10 * (value' a) + (value' b)
