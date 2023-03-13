--Wholemeal Programming Questions (#!)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3*x + 1)

--Binary Tree question (#2)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

testTree = Node 3 
  (Node 2 
    (Node 0 Leaf 'F' Leaf)
    'I' 
    (Node 1 
      (Node 0 Leaf 'B' Leaf)
      'C' 
    Leaf)
  ) 
  'J' 
  (Node 2 
    (Node 1 
      (Node 0 Leaf 'A' Leaf) 
      'G' 
      Leaf
    ) 
    'H'
    (Node 1 
      (Node 0 Leaf 'D' Leaf) 
      'E' 
      Leaf
    ))

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node h left x' right)
          | height left < height right = 
              let newLeft = insert x left
              in Node (1+max (height newLeft) (height right)) newLeft x' right
          | otherwise = 
              let newRight = insert x right
              in Node (1+max (height left) (height newRight)) left x' newRight
        height Leaf = -1
        height (Node h _ _ _) = h
  
--Xor (#3-1)



xor :: [Bool] -> Bool
xor = odd . sum . foldl (\acc x -> if x then 1:acc else acc) []

--Map using fold (#3-2)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

--FoldL using FoldR (#3-3)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

