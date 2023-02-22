zipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith _ _ [] = []
zipwith _ [] _ = []
zipwith f (x:xs) (y:ys) = (f x y) : zipwith f xs ys


quicksort ::  (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort (filter (<=x) xs)        
        larger = quicksort (filter (>x) xs)
     in smaller ++ [x] ++ larger
    