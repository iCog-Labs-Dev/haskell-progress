module Change (findFewestCoins) where
import Control.Monad (filterM)
import Data.List

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins | null smallChange = Nothing | otherwise = Just $ head smallChange
    where changes = filter (\x -> sum x == target) (powerSetOfCoins coins)
          smallChange = sortBy (\x y -> if length x >= length y then GT else LT) changes

validChange :: Integer -> [Integer] -> Bool
validChange target = any (target >=)

powerSetOfCoins :: [a] -> [[a]]
powerSetOfCoins = filterM (\_ -> [True, False]) 
