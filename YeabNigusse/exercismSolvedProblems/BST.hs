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

data BST a = EmptyTree | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node a left right) = Just left
bstLeft EmptyTree = Nothing
         

bstRight :: BST a -> Maybe (BST a)
bstRight (Node a left right) = Just right
bstRight EmptyTree = Nothing


bstValue :: BST a -> Maybe a
bstValue (Node a left right) = Just a
bstValue EmptyTree = Nothing

empty :: BST a
empty = EmptyTree

fromList :: Ord a => [a] -> BST a
fromList xs = foldr insert EmptyTree (reverse xs)

insert :: Ord a => a -> BST a -> BST a
insert x EmptyTree = singleton x
insert x (Node a left right)
          | x > a = Node a left (insert x right)
          | x <= a = Node a (insert x left) right

singleton :: a -> BST a
singleton x = Node x EmptyTree EmptyTree

toList :: BST a -> [a]
toList EmptyTree = []
toList (Node a left right) = toList left ++ [a] ++ toList right
