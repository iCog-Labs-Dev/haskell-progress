{-
Basic prelude functions implemented by fold functions.
-}

-- length of a list
length' :: [a] -> Int
length' = foldl (\acc x -> acc + 1) 0

-- plusplus' xs ys = foldl (\acc x -> append' x  acc) xs ys
plusplus' :: [a] -> [a] -> [a]
plusplus' = foldl (flip append')

-- a helper function for plusplus' (this does not use fold)
append' :: a -> [a] -> [a]
append' a [x] = [x, a]
append' a (x : xs) = x : append' a xs

-- product of a list
product' :: [Float] -> Float
product' (x : xs) = foldl (*) x xs

-- boolean or of all the values in a list
or' :: [Bool] -> Bool
or' (x : xs) = foldl (||) x xs

-- checks if any the values of a list satisfy a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' pred (x : xs) = foldl (\acc a -> acc || pred a) (pred x) xs

-- checks if all of the values of a list satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' pred (x : xs) = foldl (\acc a -> acc && pred a) (pred x) xs

-- applies a function to all the values of a list
map' :: (a -> b) -> [a] -> [b]
map' func = foldr (\x acc -> func x : acc) []

-- reverses a string
reverse' :: [a] -> [a]
reverse' (x : xs) = foldl (flip (:)) [x] xs

-- concatenates a list of lists into a single list.
concat' :: [[a]] -> [a]
concat' (x : xs) = foldl plusplus' x xs