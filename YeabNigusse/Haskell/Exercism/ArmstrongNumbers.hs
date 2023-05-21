module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong a = sum [x ^ length (toDigits (fromIntegral a))| x <- toDigits (fromIntegral a)] == fromIntegral a


toDigits :: Integer -> [Integer]
toDigits 0  = []
toDigits n 
        | n < 0 = []
        | n < 10 = [n]
        | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
