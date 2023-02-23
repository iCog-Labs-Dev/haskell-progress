-- ********* Higher Order Functions **************
-- Are functions that either take functions as parameters or return functions as return values

-- ** All functions in haskell really take only one parameter, functions that take more than 1 parameter
-- are called Curried functions
curriedFunction = (max 4) 5 == max 4 5 -- These are actually the same
-- max :: (Ord a) => a -> a -> a is the same as max :: (Ord a) => a -> (a -> a)

-- *** if we call a function with too few parameters, we get back a partially applied function, 
-- meaning a function that takes as many parameters as we left out
multThreeNos :: (Num a) => a -> a -> a -> a
multThreeNos a b c = a * b * c

multTwoWith_9 = multThreeNos 9 -- this creates a function that takes 2 nos and multiplies them with 9
multWith_18 = multTwoWith_9 2 -- this creates a fucntion that takes 1 no and multiplies it with 18
finalResult = multWith_18 10 -- this calls the function with 10 as the parameter... it returns 180

-- ** Compare with 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare x 100

compare80with100 = compareWithHundred 80
compare110with100 = compareWithHundred 110

-- ** Divide by 10 ... calling divideByTen 200 is equivalent to doing 200 / 10, as is doing (/10) 200
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
-- ** is Uppercase ... isUppercase A ---> True
isUppercase:: Char -> Bool  
isUppercase = (`elem` ['A'..'Z'])  

-- ************ Higher order functions
-- ** This is a function that takes in another function and applies it twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

hofExample1 = applyTwice (+3) 10  -- 16
hofExample2 = applyTwice (++ " HAHA") "HEY"  -- "HEY HAHA HAHA"
hofExample3 = applyTwice (3:) [1] -- [3,3,1] 

-- ** Zip with higher order functions
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
zipExample = zipWith' (+) [1,2,3,4] [5,6,7]  -- [6,8,10]

-- ******** Maps and Filters
{- map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs -}  -- recursively apply f to every element in the list

mapExample0 = map (3+) [1,2,3] -- [4,5,6]
mapExample1 = map (+3) [1,2,3] -- [4,5,6]
mapExample2 = map (replicate 3) [3..6] -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]] 
-- each of these could be achieved with a list comprehension, but 'map' is more readable
-- map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]]

-- ** Filter
{- filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  -}

filterExample1 = filter (>3) [1,5,3,2,1,6,4,3,2,1]  
filterExample2 = filter even [1..10] 
filterExample3 = let notNull x = not (null x) in filter notNull [[1,2], [3,4,5], [], []]
filterExample4 = filter (`elem` ['A'..'Z']) "How is Everything. Live LOve last" -- filter caps only
-- filter can also be writtern using list comprehension

-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter f [100000,99999..]) -- let filter f [..] in f x = x `mod` 3829
                where f x = x `mod` 3829 == 0

-- sum of all odd squares less than 10000
sumOddSquares = sum (takeWhile (<= 10000) (filter f (map (^2) [1..])))
                where f x = x `mod` 2 == 1
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  

-- ** Collatz sequence - We take a natural number. If that number is even, we divide it by two. 
-- If it's odd, we multiply it by 3 and then add 1 
collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence x
        | even x = x : collatzSequence (x `div` 2)
        | odd x = x : collatzSequence (x*3 + 1)
-- For all starting numbers between 1 and 100, how many chains have a length greater than 15
collatzTo100 :: Int -- because 'length' returns Int instead of Num a
collatzTo100 = length (filter isLong (map collatzSequence [1..100]))
            where isLong x = length x > 15
-- **** Note that: map (*) [0..] returns a list of functions that take one parameter -> [(0*),(1*),(2*),...
listOfFunctions = map (*) [0..]
index4 = (listOfFunctions !! 4) 5 -- (4*) 5 = 20

-- ********** Lambdas
--  make a lambda, we write a \ and then we write the parameters, separated by spaces.
-- After that comes a -> and then the function body. 
-- We usually surround them by parentheses, because otherwise they extend all the way to the right.
collatzTo101 :: Int
collatzTo101 = length (filter (\x -> length x > 15) (map collatzSequence [1..101]))
-- lambdas can take more than one parameter
lambdaExample1 = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  -- zipWith (f [...] [...])
-- you can pattern match with lambdas but you can't match them with multiple patterns
lambdaExample2 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  -- [3,8,9,8,7]
lambdaExample3 = map (\(x:xs) -> x) [[1,2],[3,4],[5,6],[7,8]] -- [1,3,5,7]
-- This is to illustrate currying. Here, 'x', 'y', 'z' are all partial functions that take the one to the right as a parameter
addThree :: (Num a) => a -> a -> a -> a   
addThree = \x -> \y -> \z -> x + y + z  -- is the same as: addThree x y z = x + y + z
-- Another example of lambdas
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x  -- Same as: flip' f x y = f y x

-- ** fold functions -  fold takes a binary function (\...), a starting value (0) and a list to fold up (list).
-- The binary function itself takes two parameters, the accumulator (acc) and the first(or last) element / current 
-- value (x) and produces a new 'acc'. Then, the binary function is called again with the new 'acc' and the new 'x', and so on.
sumFoldl :: (Num a) => [a] -> a 
sumFoldl list = foldl(\acc idx -> acc + idx) 0 list
sumFoldlExample = sumFoldl [1,2,3,4] -- 10  
-- a more brief version of sum using foldl .... foo a = bar b a is the same as foo = bar b
sum' = foldl (+) 0 -- (\acc x -> acc + x) is the same as (+)
sumFoldlExample2 = sum' [1,2,3,4,5] -- 15
-- Now, lets implement 'elem' using foldl
elem' :: (Eq a) => a -> [a] -> Bool
elem' a list = foldl(\acc x -> if x==a then True else acc) False list 
-- the type of the return value and the 'acc' should be the same. Here we are changing the value of the 'acc'
-- to True if we find a match and leave it unchanged if a match is not found.

-- ** The right fold, foldr works similarly to left fold, only the accumulator takes the values from the right. 
-- foldr binary function has the current value as the first parameter and the accumulator as the second one
-- (\x acc -> ...)
-- Implementing the 'map' function with right fold
mapr' :: (a -> b) -> [a] -> [b]  
mapr' f xs = foldr (\x acc -> f x : acc) [] xs 
-- 'map' with left fold
mapl' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with a
-- starting value. They assume the first (or last) element of the list to be the starting value and then start
-- the fold with the element next to it.
-- 'sum' implemented with foldl1 would be like this:
sumFoldl1 :: (Num a) => [a] -> a  
sumFoldl1 = foldl1 (+) -- foldl1 (\acc x -> acc + x) list    ... however, this causes a runtime error if called with an emty list

-- Here are some examples with fold
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) [] -- take a starting value of an empty list and then approach the list from
-- the left and just prepend to our accumulator.
reverse2 :: [a] -> [a] 
reverse2 = foldl (flip (:)) [] -- simpler way to implement reverse using fold

filterFoldl :: (a -> Bool) -> [a] -> [a]  
filterFoldl cond = foldl (\acc x -> if cond x then x:acc else acc) []

head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  -- return the value at the last iteration from the right
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  -- return the value at the last iteration from the left

-- *** scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states
-- in the form of a list. scanl1 and scanr1, are analogous to foldl1 and foldr1
scanl1Example = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] -- [3,4,5,5,7,9,9,9]  
scanrExample = scanr (+) 0 [3,5,2,1] -- [11,8,3,1,0]
-- When using a scanl, the final result will be in the last element of the resulting list while a scanr will place the result in the head.

-- : How many elements does it take for the sum of the roots of all natural numbers to exceed 1000
sumRoots :: Int
sumRoots = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
 -- We use takeWhile here instead of filter because filter doesn't work on infinite lists

 -- **** Function application with $
 -- the $ function has the lowest precedence. Function application with a space is left-associative
 -- (so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.

funcAppnExample = sqrt $ 3+4+5 -- sqrt (3+4+5)
largeSquareSum = sum $ filter (>10) $ map (*2) [2..10] -- sum (filter (> 10) (map (*2) [2..10]))

-- **** Function Composition .... (f.g)(x) = f(g(x)).
-- The expression 'negate . (* 3)' returns a function that takes a number,-- multiplies it by 3 and then negates it.
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  

-- funcCompExample1 = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  -- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
-- funcCompExample2 = map (negate . sum . tail) [[1..5],[3..6],[1..7]]  -- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]] 
-- For functiona that take more than one param, we will partially apply them so that they'll take 1 param
-- funcCompExample3 = (sum . replicate 5 . max 6.7) 8.9 -- sum (replicate 5 (max 6.7 8.9))
-- funcCompExample4 = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8] -- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
