module ResistorColors (Color(..), Resistor(..), label, ohms) where

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

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = case zeroDigits of
        0 -> show digit ++ " " ++ "ohms"
        1 -> show digit ++ " " ++ "kiloohms"
        2 -> show digit ++ " " ++ "megaohms"
        3 -> show digit ++ " " ++ "gigaohms"
    where (digit, zeroDigits) = convertToSmallNumber (ohms resistor) 0

ohms :: Resistor -> Int
ohms (Resistor (a,b,c)) = read (show (getColorCode a) ++ show (getColorCode b) ++ replicate (getColorCode c) '0')

getColorCode :: Color -> Int
getColorCode color = let (Just digit) = lookup color colorDigit in digit
    where colorDigit :: [(Color, Int)]
          colorDigit = foldl (\acc x -> let (_, prevValue) = head acc in (x,prevValue + 1): acc) [(Black, 0)] [Brown .. White]

convertToSmallNumber :: Int -> Int -> (Int, Int)
convertToSmallNumber num count
    | num == 0 = (0,0)
    | num `mod` 1000 == 0 = convertToSmallNumber (num `div` 1000) (count+1)
    | otherwise = (num, count)