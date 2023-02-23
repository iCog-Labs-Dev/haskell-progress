myMaximum :: (Ord a) => [a] -> a
myMaximum (x:[]) = x
myMaximum (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = myMaximum xs