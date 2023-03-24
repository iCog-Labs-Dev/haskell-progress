module Golf where 
import Data.List (findIndex, elemIndex, group, sort, transpose)

skips :: [a] -> [[a]]
skips xs = init $ foldr (\x acc -> getEveryN xs x 0 : acc) [[]] [1..length xs]
    where getEveryN :: [a] -> Int -> Int -> [a]
          getEveryN xs 1 _ = xs
          getEveryN xs n index
            | index < length xs = if (index+1) `mod` n == 0 
                                    then xs !! index : getEveryN xs n (index + 1) 
                                    else getEveryN xs n $ index + 1
            | otherwise = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldr (\x acc -> if isMaximum x xs then xs !! x : acc else acc) [] [0..length xs - 1]
    
    where isMaximum :: Int -> [Integer] -> Bool
          isMaximum index nums
            | index-1 < 0 = False
            | index+1 >= length nums = False
            | otherwise = nums !! (index - 1) < n && nums !! (index + 1) < n
            
            where n = nums !! index

histogram :: [Int] -> String
histogram xs = unlines (histRows ++ [histBottom])
    where counts = map (\n -> length $ filter (==n) xs) [0..9]
          maxCount = maximum counts
          histRows = reverse $ transpose $ map (\c -> replicate c '*' ++ replicate (maxCount - c) ' ') counts
          histBottom = replicate 10 '=' ++ "\n" ++ concatMap show [0..9]