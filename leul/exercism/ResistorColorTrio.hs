-- https://exercism.org/tracks/haskell/exercises/resistor-color-trio/

module ResistorColors (Color(..), Resistor(..), label, ohms) where


import Data.List (intercalate)


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
  deriving (Show, Enum, Bounded)


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
            

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

group3 :: String -> [String]
group3 [] = []
group3 xs = (take 3 xs) : group3 (drop 3 xs)
          
unit :: Int -> String
unit n = case n of 
                0 -> "ohms"
                1 -> "kiloohms"
                2 -> "megaohms"
                3 -> "gigaohms"                


label :: Resistor -> String
label resistor = intercalate "" (map reverse (reverse nonZeros)) ++ " " ++ valUnit
    where val = ohms resistor
          grouped = (group3 . reverse . show) val
          nonZeros = filter (/="000") grouped
          valUnit = unit $ (length grouped) - (length nonZeros)


ohms :: Resistor -> Int
ohms resistor = 10 ^ (value' zeros) * (10 * (value' a) + (value' b) )
    where (a, b, zeros) = bands resistor
