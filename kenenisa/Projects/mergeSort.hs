
mergeSort :: Ord a => [a] -> [a]
mergeSort x 
    | length x < 2 = x
    | otherwise = merge (mergeSort $ fst sp) (mergeSort $ snd sp)
        where 
            middle = length x `div`2
            sp = splitAt middle x  
            merge [] [] = []
            merge xs [] = xs
            merge [] ys = ys 
            merge xx@(x:xs) yy@(y:ys) 
                | x <= y = x : merge xs yy
                | otherwise = y : merge xx ys              

main = do 
    let m = mergeSort (replicate (10^7) 100)
    print $ length m