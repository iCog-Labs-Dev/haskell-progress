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

value :: (Color, Color) -> Int
value (a, b) = read (show (getColorCode a) ++ show (getColorCode b))

colorDigit :: [(Color, Int)]
colorDigit = foldl (\acc x -> let (_, prevValue) = head acc in (x,prevValue + 1): acc) [(Black, 0)] [Brown .. White]

getColorCode :: Color -> Int
getColorCode color = let (Just digit) = lookup color colorDigit in digit