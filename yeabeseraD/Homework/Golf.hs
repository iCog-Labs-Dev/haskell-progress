module Golf where 
import Data.List (findIndex, elemIndex)

skips :: [a] -> [[a]]
skips xs = init $ foldr (\x acc -> getEveryN xs x 0 : acc) [[]] [1..length xs]
    where getEveryN :: [a] -> Int -> Int -> [a]
          getEveryN xs 1 _ = xs
          getEveryN xs n index
            | index < length xs = if (index+1) `mod` n == 0 then xs !! index : getEveryN xs n (index + 1) else getEveryN xs n (index + 1)
            | otherwise = []
