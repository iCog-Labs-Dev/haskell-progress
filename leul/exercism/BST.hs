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

data BST a = Empty | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node left _ _) = Just left


bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = Just right


bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node _ val _) = Just val


empty :: BST a
empty = Empty


fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty . reverse


insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x <= val = Node (insert x left) val right
    | otherwise = Node left val (insert x right)


singleton :: a -> BST a
singleton x = Node Empty x Empty


toList :: BST a -> [a]
toList Empty = []
toList (Node left val right) = toList left ++ [val] ++ toList right
