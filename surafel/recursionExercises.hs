{-
Contains basic prelude and other common functions implemented by recursion .
-}

-- returns the nth fibonacci number
fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n - 2) + fib' (n - 1)

-- returns the maximum from a list
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x : xs)
  | x > maximum' xs = x
  | otherwise = maximum' xs

-- takes an Int and any other type and returns a list of the type with length Int
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' x a = a : replicate' (x - 1) a

-- takes and Int and a list then returns the first Int occurences
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' y (x : xs) = x : take' (y - 1) xs

-- reverses a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- repeat something infinite times
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- zip
-- coming soon

-- quick sort

quickSort [] = []
quickSort (x : xs) = quickSort left ++ [x] ++ quickSort right
  where
    left = filter (< x) xs
    right = filter (> x) xs

-- binary trees
data Tree v = EmptyTree | Node v (Tree v) (Tree v) deriving (Show, Read, Eq)

createTree :: Ord a => [a] -> Tree a
createTree [] = EmptyTree
createTree xs = Node (middle xs) (createTree (left xs)) (createTree (right xs))
  where
    middle :: [a] -> a
    middle xs = xs !! (length xs `div` 2)
    left :: Ord a => [a] -> [a]
    left xs = filter (< middle xs) xs
    right :: Ord a => [a] -> [a]
    right xs = filter (> middle xs) xs