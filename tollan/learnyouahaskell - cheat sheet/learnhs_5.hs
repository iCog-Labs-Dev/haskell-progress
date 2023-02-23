-- ******* Recursion
-- ** Maximum number in a list
maxInList :: (Ord a) => [a] -> a
maxInList [] = error "Error, max of empty list"
maxInList [max] = max
maxInList (max:tail)
    | max > maxTail = max
    | otherwise = maxTail
    where maxTail = maxInList tail
-- using the 'max' function that finds the maximum of two numbers
simpleMaxInList :: (Ord a) => [a] -> a
simpleMaxInList [] = error "Error, max of empty list"
simpleMaxInList [x] = x
simpleMaxInList (x:xs) = max x (simpleMaxInList xs)

-- -- ** Replicate a number 'm' n times
replicateNM :: (Num a, Ord a) => a -> i -> [i] -- Num is not a subclass of Ord. We have to specify both 
-- Num and Ord class constraints when doing addition or subtraction (Num) and also comparison (Ord).
replicateNM n m
    | n <= 0    = [] -- We used guards here instead of patterns because we're testing for a boolean condition
    | otherwise = m:replicateNM (n-1) m

-- ** Take some elements from the beginning of a list
takeFromList :: (Num a, Ord a) => a -> [i] -> [i]
takeFromList n list
    | n <= 0 = [] -- Edge case 1: if n <= 0. Requires comparison, so we use guards
takeFromList _ [] = [] -- Edge case 2: if the list is empty. We can easily pattern match to this
takeFromList n (x:listTail) = x:takeFromList (n-1) listTail -- append the first element to the function being called recursively 'n' times
-- we can also do this, but we wanted to match with the second edge case
takeFromList2 :: (Num a, Ord a) => a -> [i] -> [i]
takeFromList2 n (x:listTail)
    | n <= 0 = [] 
    | otherwise = x:takeFromList2 (n-1) listTail 
 
-- ** Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- ** Repeat a number indefinitely in a list
repeatNumber :: a -> [a]
repeatNumber a = a:repeatNumber a
-- Haskell supprots infinite lists so we can use the 'repeat' function with 'take' and it would be like 'replicate'
takeRepeat = takeFromList 3 (repeatNumber 5) -- [5,5,5]

-- ** ZIP / join two lists together as tuples in a list, the length of the shorter list will be applied for both lists
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  -- zip' [1,2,3] [2,3] returns [(1,2),(2,3)]

-- ** Check if an element exists in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    | x == n = True
    | otherwise = n `elem'` xs -- we can also call the function as an infix

-- ** Quick sort using recursion
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =   
    let smallerSorted = quickSort [a | a <- xs, a <= x]  
        biggerSorted = quickSort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  