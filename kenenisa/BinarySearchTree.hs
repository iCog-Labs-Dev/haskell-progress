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

data BST a = Leaf | Node a (BST a) (BST a) deriving (Show, Read, Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Node a left right) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Node a left right) = Just right

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Node a left right) = Just a


empty :: BST a
empty = Leaf

createTree :: Ord a => [a] -> BST a -> BST a
createTree xs tree = foldl (flip insert) tree xs

fromList :: Ord a => [a] -> BST a
fromList xs = createTree xs Leaf

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node a left right)
       | x > a = Node a left (insert x right)
       | x <= a = Node a (insert x left) right

singleton :: a -> BST a
singleton x = Node x Leaf Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Node a left right) = toList left ++ [a] ++ toList right
