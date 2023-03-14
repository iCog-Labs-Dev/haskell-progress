-- https://exercism.org/tracks/haskell/exercises/yacht/

module Yacht (yacht, Category(..)) where
import Data.List (nub, sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht


yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones -> calculateNs 1 dice
    Twos -> calculateNs 2 dice
    Threes -> calculateNs 3 dice
    Fours -> calculateNs 4 dice
    Fives -> calculateNs 5 dice
    Sixes -> calculateNs 6 dice
    FullHouse -> calculateFullHouse dice
    FourOfAKind -> calculateFourOfAKind dice
    LittleStraight -> calculateLittleStraight dice
    BigStraight -> calculateBigStraight dice
    Choice -> calculateChoice dice
    Yacht -> calculateYacht dice

calculateYacht :: [Int] -> Int
calculateYacht xs 
    | (length . nub) xs == 1 = 50
    | otherwise = 0

calculateChoice :: [Int] -> Int
calculateChoice = sum

calculateBigStraight :: [Int] -> Int
calculateBigStraight xs
    | sort xs == [2,3,4,5,6] = 30
    | otherwise = 0


calculateLittleStraight :: [Int] -> Int
calculateLittleStraight xs
    | sort xs == [1,2,3,4,5] = 30
    | otherwise = 0


calculateFourOfAKind :: [Int] -> Int
calculateFourOfAKind xs = sum $ take 4 [x | x <- xs, count x xs >= 4]


calculateFullHouse :: [Int] -> Int
calculateFullHouse xs 
    | (length . nub) xs /= 2 = 0
    | count (head xs) xs `elem` [2, 3] = sum xs
    | otherwise = 0



calculateNs :: Int -> [Int] -> Int
calculateNs n xs = n * count n xs


count :: (Eq a) => a -> [a] -> Int
count y xs = sum [1 | x <- xs, x == y]