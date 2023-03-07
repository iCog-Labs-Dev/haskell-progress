
xor' :: Bool -> Bool -> Bool
xor' a b = (a  && b) /= (a || b)

xor :: [Bool] -> Bool
xor (x:xs) =  foldr xor' x xs


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\val acc -> f val : acc) []


filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\val acc -> if f val then val:acc else acc) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\val acc -> f acc val) base xs