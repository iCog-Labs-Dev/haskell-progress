module SecretHandshake (handshake) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

handshake :: Int -> [String]
handshake n
         | n > 15 && toBinary n !! 4 == '1' = reverse (toString (myList (toBinary n)))
         | otherwise = toString (myList (toBinary n))


toBinary :: Int -> String
toBinary n = reverse (showIntAtBase 2 intToDigit n "")

code :: (Char, Int) -> String
code (x, index)
       | x == '1' && index == 0 = "wink"
       | x == '1' && index == 1 = "double blink"
       | x == '1' && index == 2 = "close your eyes"
       | x == '1' && index == 3 = "jump"
       | otherwise = ""
       

myList :: String -> [(Char, Int)]
myList str = zip str [0 .. length str]

toString :: [(Char, Int)] -> [String]
toString [] = []
toString (x:xs) = filter (not . null) (code x : toString xs)

