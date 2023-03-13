data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)


depth :: BinaryTree a -> Int
depth Empty = 0
depth (Node left val right) = max (depth left) (depth right) + 1


instance Functor BinaryTree where
    fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
    fmap f Empty = Empty
    fmap f (Node left val right) = Node (fmap f left) (f val) (fmap f right)
    

    
left :: BinaryTree Integer
left  = Node Empty 1 Empty 

right :: BinaryTree Integer
right   = Node Empty 2 Empty 

root :: BinaryTree Integer
root  = Node left 3 right



