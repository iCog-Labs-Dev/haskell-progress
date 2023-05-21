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
value (a, b) = read (toString resistor a b)


resistor :: [(Color, Char)]
resistor = [(Black, '0'),(Brown, '1'),(Red, '2'),(Orange, '3'),(Yellow, '4'),
            (Green, '5'),(Blue, '6'),(Violet, '7'),(Grey, '8'),(White, '9')]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd .head . filter (\(k,v) -> k == key) $ xs 


toString :: [(Color, Char)] -> Color -> Color -> [Char]
toString resistor a b = (findKey a resistor) : [(findKey b resistor)]