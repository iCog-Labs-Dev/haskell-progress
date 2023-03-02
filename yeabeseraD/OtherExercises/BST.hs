data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

getNode:: Ord a => a->Tree a 
getNode x = Node x Empty Empty

insertNode:: (Ord a) => a -> Tree a -> Tree a 
insertNode x Empty = getNode x
insertNode x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (insertNode x left) right
    | x > a = Node a left (insertNode x right)

elemT :: (Ord a) => a -> Tree a -> Bool
elemT _ Empty = False
elemT x (Node a left right)
    | x == a = True
    | x < a = elemT x left
    | x > a = elemT x right

inorder :: (Show a) => Tree a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertNode Empty