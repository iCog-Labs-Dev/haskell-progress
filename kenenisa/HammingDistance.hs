module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs /= length ys = Nothing
               | otherwise = Just $ calcDistance xs ys
                  where calcDistance [] [] = 0
                        calcDistance (x:xss) (y:yss) | x /= y = 1 + calcDistance xss yss
                                                     | otherwise = calcDistance xss yss 
  
