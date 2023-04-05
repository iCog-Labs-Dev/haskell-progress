module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = error "You need to implement this function."

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = error "You need to implement this function."

length :: [a] -> Int
length xs = sum [1| _ <- xs]

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
       | p x = x : filter p xs
       | otherwise = filter p xs

(++) :: [a] -> [a] -> [a]
xs ++ [] = xs
[] ++ ys = ys
xs ++ ys = error "You need to implement this function."

concat :: [[a]] -> [a]
concat [[]] = []
concat (xs:xss) = xs ++ concat xss
