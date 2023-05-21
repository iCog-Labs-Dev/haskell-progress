{-
1. Using library functions, define a function halve :: [a] -> ([a],[a]) that
splits an even-lengthed list into two halves. For example:
> halve [1,2,3,4,5,6]
([1,2,3],[4,5,6])
-}

halve :: [a] -> ([a],[a])
halve xs = let half = length xs `div` 2
           in (take half xs, drop half xs)

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (x:y:z:rest) = z

null' :: [a] -> Bool
null' = null 

safetail :: [a] -> [a]
safetail xs 
         | null' xs = []
         | otherwise = tail xs 

safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs


bool :: Bool -> Bool -> Bool
bool a b = if a then if b then True else False else False

bool' :: Bool -> Bool -> Bool
bool' a b = if a then b else a

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then x*2 -9 else x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0