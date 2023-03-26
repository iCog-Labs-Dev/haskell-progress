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
  deriving (Show, Enum, Bounded,Eq)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving (Show, Eq)

label :: Resistor -> String
label resistor = powermessage  (ohms resistor) resistor

ohms :: Resistor -> Int
ohms (Resistor (a,b,c)) = read (toString resistor a b) * power c resistor


resistor :: [(Color, Char)]
resistor = [(Black, '0'),(Brown, '1'),(Red, '2'),(Orange, '3'),(Yellow, '4'),
            (Green, '5'),(Blue, '6'),(Violet, '7'),(Grey, '8'),(White, '9')]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd .head . filter (\(k,v) -> k == key) $ xs 
            
            
toString :: [(Color, Char)] -> Color -> Color -> [Char]
toString resistor a b = (findKey a resistor) : [(findKey b resistor)]

power :: Color ->[(Color, Char)] -> Int
power a resi = 10 ^ read [findKey a resi]


powermessage :: Int -> Resistor -> String
powermessage n resistor
             | n == 0 = "0 ohms" 
             | n `mod` 10^9 == 0 = show  (n `div` (10^9)) ++" gigaohms"
             | n `mod` 10^6 == 0 = show  (n `div` (10^6)) ++ " megaohms"
             | n `mod` 10^3 == 0 = show  (n `div` (10^3)) ++" kiloohms"
             | otherwise = show (ohms resistor) ++ " ohms"         
            