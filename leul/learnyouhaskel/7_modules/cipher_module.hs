module Cipher (ceasarCipher) where

import Data.Char (chr, ord)

ceasarCipher :: Int -> [Char] -> [Char]
ceasarCipher key = map (chr . \char -> ord char + key)