type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = []

moveDisk :: Int -> [Int]
moveDisk n
    | n < head firstPeg = n : firstPeg

first :: Int -> [Int]
first n 
    | n < head firstPeg = n : firstPeg
    | otherwise = firstPeg

firstPeg = [] :: [Int]