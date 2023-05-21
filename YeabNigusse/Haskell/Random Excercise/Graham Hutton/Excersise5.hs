sumOfSquares :: Int
sumOfSquares = sum [x^2 | x <- [1 .. 100]]

grid :: Int -> Int -> [(Int,Int)]
grid xs ys = [(x,y)| x <- [0 .. xs], y <- [0 .. ys]]

square :: Int -> [(Int,Int)]
square x = grid x x

replicate' :: Int -> a -> [a]
replicate' x y = [y | _ <- [1 .. x]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | z <- [1 .. 10], y <- [1 .. 10], x <- [1 .. 10], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects xs = [x | x <- [1 .. xs], (sum [y| y <- [1 .. x-1], x `mod` y == 0]) == x]

-- [(x,y) | x <- [1,2], y <- [3,4]]

tuple :: [(Int, Int)]
tuple = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum $ zipWith (\x y -> x*y) xs ys

