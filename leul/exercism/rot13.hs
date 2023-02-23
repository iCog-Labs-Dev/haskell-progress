module RotationalCipher (rotate) where
import Data.List

lowerLetters = ['a'..'z']
upperLetters = ['A'..'Z']

fromJust (Just a) = a

lowerIndex char = fromJust $ findIndex (==char) lowerLetters
upperIndex char = fromJust $ findIndex (==char) upperLetters

rotateChar key char 
  | char `elem` lowerLetters = lowerLetters !! ((lowerIndex char + key) `mod` 26)
  | char `elem` upperLetters = upperLetters !! ((upperIndex char + key) `mod` 26)
  | otherwise = char

rotate :: Int -> String -> String
rotate key string = [rotateChar key char | char <- string]
