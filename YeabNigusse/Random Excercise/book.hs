lastButOne :: [a] -> a
lastButOne xs = last (init xs)

-- find whether an element if found in a list or not
findElem :: (Eq a) => a -> [a] -> Bool
findElem element [] = False
findElem element (x:xs) = if x == element then True else findElem element xs

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = if findElem x xs then nub' xs else x:nub' xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = if x >= y then False else isAsc xs

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = False
hasPath list x y = 