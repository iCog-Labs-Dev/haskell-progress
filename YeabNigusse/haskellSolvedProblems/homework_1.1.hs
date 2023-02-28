-- this is a collection of a functions to help the varification of credit card 
-- first step convert credit card number into list of numbers by using toDigit Function

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
        | n < 0 = []
        | n < 10 = [n]
        | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


-- second step douple every even index of a list
-- we can write that using list comprehension by usin zip- which creat a tuple using two conseqative lists

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [if even i then 2*x else x | (i, x) <- zip [0..] xs]

-- the last step is that adding each and every element of a list found in a list 
-- if the element is more than one digit sparet and add them independently 

sumDigits  :: [Integer] -> Integer
sumDigits  [] = 0
sumDigits  (x:xs) 
     | x > 9 = (x `div` 10) + (x `mod` 10) + sumDigits  xs
     | otherwise = x + sumDigits  xs


-- one more step is tha validate wether th credit card is valid or not 
-- let us do that

validate :: Integer -> Bool
validate x = x `mod` 10 == 0

-- validate (sumDigits (doubleEveryOther (toDigits 4012888888881881)))
-- validate (sumDigits (doubleEveryOther (toDigits 4012888888881882)))
-- use the above code to test the functions,,, the anwer shoild be True and False Respectively