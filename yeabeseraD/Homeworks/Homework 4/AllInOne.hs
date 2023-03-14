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
fun2 n module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop
    | start == stop = [songList !! (start-1)]
    | otherwise = reverse $ recite (start+1) stop

songList = [
    "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.",
    "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.",
    "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    ]
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

