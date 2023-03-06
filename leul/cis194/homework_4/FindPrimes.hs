
findPrimes :: Integer -> [Integer]
findPrimes n = 2 : map (\x -> x * 2 + 1) (takeWhile (<n) [x | x <- [1..], x `notElem` toBeRemoved])
    where toBeRemoved = takeWhile (<n) $ 
                            map (\(i, j) -> i + j + 2 * i * j) $ 
                                filter (uncurry (<=)) [(i, j) | i <- [1..], j <- [1..]]