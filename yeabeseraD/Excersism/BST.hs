module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Nil | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node left _ _) = Just left
bstLeft Nil = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ right) = Just right
bstRight Nil = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node _ curr _) = Just curr 
bstValue Nil = Nothing

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x (Node left a right)
    | x <= a = Node (insert x left) a right
    | otherwise =Node left a (insert x right)

singleton :: a -> BST a
singleton x = Node Nil x Nil

toList :: BST a -> [a]
toList Nil = []
toList (Node left x right) = toList left ++ [x] ++ toList right
