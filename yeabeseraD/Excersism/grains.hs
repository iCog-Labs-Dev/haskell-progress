module Grains (square, total) where
import Data.Maybe (fromMaybe)

square :: Integer -> Maybe Integer
square n
    | n < 1 || n > 64 = Nothing
    | otherwise = Just $ squareHelper n

squareHelper 1 = 1
squareHelper n = 2 * squareHelper (n-1)

total :: Integer
total = sum (map squareHelper [1..64])