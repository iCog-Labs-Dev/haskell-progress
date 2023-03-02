data LinkedList a = Empty | Node a (LinkedList a) deriving (Eq, Ord)

instance Show a => Show (LinkedList a) where
    show :: LinkedList a -> String    
    show Empty = "End"        
    show (Node x y) = show x ++ " -> " ++ show y


insertBefore :: a -> LinkedList a -> LinkedList a
insertBefore value Empty = Node value Empty
insertBefore value node = Node value node

insertAfter :: a -> LinkedList a -> LinkedList a
insertAfter value Empty = Node value Empty
insertAfter value (Node val next) = Node val (insertBefore value next)

insertLast :: a -> LinkedList a -> LinkedList a
insertLast value Empty = Node value Empty
insertLast value (Node val next) = Node val (insertLast value next)

fromList' :: [a] -> LinkedList a
fromList' = foldr insertBefore Empty

getByIndex :: LinkedList a -> Int -> Maybe (LinkedList a)
getByIndex Empty _ = Nothing
getByIndex node 0 = Just node
getByIndex (Node val next) index = getByIndex next (index - 1)

takeTillIndex :: LinkedList a -> Int -> LinkedList a
takeTillIndex Empty _ = Empty
takeTillIndex (Node val next) 0 = Node val Empty
takeTillIndex (Node val next) index = Node val (takeTillIndex next (index - 1))

insertAtIndex :: a -> Int -> LinkedList a -> LinkedList a
insertAtIndex value _ Empty = Empty
insertAtIndex value 0 (Node val next)= Node val (insertBefore value next)
insertAtIndex value index (Node val next) = Node val (insertAtIndex value (index - 1) next)

