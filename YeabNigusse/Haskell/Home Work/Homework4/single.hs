
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

--Exersise 1
fun1' :: [Integer] -> Integer
fun1' xs =  foldl (\acc x -> (x-2)* acc) 1 [ x| x <- xs, even x]

fun2' :: Integer -> Integer
fun2' n = sum (takeWhile (/=0) $ iterate (`div` 2) n ) + fun2' (3*foldr 
                                                                   (\ x acc -> if even x then x else acc) 1 
                                                                   (takeWhile (/=0) $ iterate (`div` 2) n) + 1)

--Exercise 2: Folding with trees


-- Excersise 3 More folds!

xor :: [Bool] -> Bool
xor xs = odd (foldl (\acc x -> acc + x) 0 (map (\x -> if x == True then 1 else 0) xs))

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc ) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4: Finding primes

