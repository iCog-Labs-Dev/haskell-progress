import Data.Char (toLower)


max' :: Integer -> Integer -> Integer
max' x y = if x > y then x else y -- this shows multiple argument function which is curried function

-- let us write a function that multiply 3 numbers

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


{--
let us appliy the concept of partially applied function meaning return a function that do somthing

let multTwoWithNine = multThree 9  
let multThreeWith2 = multTwoWithNine 3
multThreeWith2 2

in the above code we are defining one function based on the privious function 
--} 

-- write partiall applied function that compare numbers with 100

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 

-- write partiall applied function that devide a number by 10

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- SOME HIGHER OREDER FUNCTIONS
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ZIPWITH => a standard librery function that take two lists with function and apply the function on two lists
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

-- HERE IS A FUNCTION THAT SWAP TWO NUMBERS
flip' :: a -> b -> (b, a)
flip' a b = (b , a)

-- MAP => standard librery function thate take a function and a list and applie the function on the list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- THIS IS FILTER FUNCTION WICH TAKES BOOLIAN FUNCTION AND LIST AND APPLIE THE FUNCTION ON THE LIST
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
        | f x = x : filter' f xs
        | otherwise = filter' f xs

-- let us do something with takeWhile

add :: Integer
add = sum (takeWhile (<10000) (filter' odd (map' (^2) [1 ..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
      | even n = n:chain(n `div` 2)
      | odd n = n:chain(n*3 +1)

isLong ::[a] -> Bool
isLong x = length x > 15

numLength :: Int
numLength = length (filter isLong (map' chain [1,2 .. 100]))

isFound :: String -> Char -> Bool
isFound text x = x `elem` text

lowerCaseText :: String -> String
lowerCaseText = map toLower

-- LAMBDAS
numLengthChains :: Int
numLengthChains = length (filter (\x -> length x > 15) (map' chain [1,2..100]))
-- we use lambadas to write a functio that we use onece or twice 


addthree :: [Integer]
addthree = map (\x -> x+3) [1,2,3,4]

multiply :: [Integer]
multiply = zipWith (\x y -> x*y) [1,2,3,4,5] [1,2,3,4,5]

-- SUM OF EVEN SQUARES IN THE RANGE OF 1 UPTO 10 USING LAMBDAS
sumEvenSquares ::  Int
sumEvenSquares = sum (map (\x -> x^2) (filter even [1,2..10]))

-- DOING THE ABOVE PROBELM USING FILTER AND MAP FUNCTION
evenSquareSum' ::  Int
evenSquareSum' = sum (takeWhile (<10000) (filter even (map (^2) [1 ..])))


-- FOLDEL
sum' :: Integer
sum' = foldl (\acc x -> acc + x) 0 [1,2,3,4,5,6]
-- LET US DO SOME FOLDERING
-- FOLDR FUNCTION IS USED TO OPERATE A FUCTION ON TO LIST
-- IT TAKES THE FUNCTION ACCUMULLETER AND THE LIST

-- LET US REVERSE A LIST USING fldr


reverseList :: [a] -> [a]
reverseList lst = foldr (\x acc -> acc ++ [x]) [] lst

concatString :: [String] -> String
concatString = foldr (\x acc -> x ++ acc) ""

-- FUNCTION APPLICATION WITH $ - increase readebility of code by reducing parentesis 
-- let us write the sumEvenSquares Function using function appliction
sumEvenSquares'' :: Int
sumEvenSquares'' = sum $ map' (\x -> x^2) $ filter even [1,2..10]

-- LET US WRITE SOME COMPOSITION FUNCTION

negatNums :: [Integer]
negatNums = map (\x -> negate (abs x)) [1,2..10] -- this is using lambdas 
-- the above code can be simplified as
negatNums' :: [Integer]
negatNums' = map (negate . abs ) [1,2 .. 10]

oddSquareSum :: Int
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1 ..])))
 -- THE ABOVE CODE CAN WRITEN AS USING COPOSITION FUNCTION

oddSquareSum' :: Int
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1 ..] 