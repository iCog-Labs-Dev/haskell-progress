factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

fibonnaci :: (Integral a) => a -> a
fibonnaci 0 = 0
fibonnaci 1 = 1
fibonnaci x = fibonnaci (x - 2) + fibonnaci (x - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

length' :: (Num a) => [a] -> a
length' [] = 0
length' (x:y) = 1 + length' y
