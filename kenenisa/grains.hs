module Grains (square, total) where
-- square :: (Num (Maybe a), Integral b) => b -> Maybe a
square :: (Num a, Integral b) => b -> Maybe a
square n | n > 0 && n <= 64 = Just (2 ^ (n-1)) | otherwise = Nothing
onSquare :: Integer -> Integer -> Integer
onSquare t 0 = t 
onSquare t n = t + onSquare (2^n) (n-1)
total :: Integer
total = onSquare 1 63
