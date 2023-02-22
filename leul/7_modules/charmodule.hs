import Data.Char

ceasarCipher :: Int -> [Char] -> [Char]
ceasarCipher key = map (chr . \char -> ord char + key)