-- MAXIMUM AWESOME


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
         | x > maximum' xs = x
         | otherwise = maximum' xs
-- we can do this also by using max function

max' :: (Ord a) => [a] -> a
max' [] = error "Empty list"
max' (x:xs) = max x (max' xs)

--A few more recursive functions

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
           | n <= 0 = []
           | otherwise = x:replicate' (n-1) x
take' ::    (Num i, Ord i) => i -> [a] -> [a]
take' _ []     = []  
take' n (x:xs)
      | n <= 0 = []
      | otherwise = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y: ys) = (x,y): zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
        | a == x = True
        | otherwise = elem' a xs

-- Quick, sort!

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = let smallSorted = quickSort [a | a <- xs, a <= x]
                       biggerSorted = quickSort [a | a <- xs, a > x]
                       in smallSorted ++ [x] ++ biggerSorted
    