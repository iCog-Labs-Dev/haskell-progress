import Data.List(sortBy)

length' :: [a] -> Integer
length' xs = sum [1 | x<- xs]

mean xs = sum [x | x<- xs]/sum [1 | x<- xs]

intoPlaindrom xs = xs ++ reverse xs

isPalindrom xs = xs == reverse xs

intersperse :: a -> [[a]] -> [a]
intersperse y (x:xs) = x ++ [y] ++ intersperse xs 