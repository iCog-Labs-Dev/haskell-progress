module JoinList where
import Control.Monad.RWS (Product(Product))
import Sized

data JoinList m a = Empty
                     | Single m a
                     | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty l1    = l1
(+++) l1 Empty    = l1
(+++) l1 l2       = Append (tag l1 <> tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x y) = x
tag (Append x _ _) = x

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                            = Nothing
indexJ i _ | i < 0                        = Nothing
indexJ i (Single size' y) | i == kl       = Just y
    where kl = getSize $ size size'
indexJ i jl@(Append b l1 l2)
    | i < kl                              = indexJ i l1
    | i > kl                              = indexJ i l2
    where kl = getSize $ size $ tag jl

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                             = Empty
dropJ i tree | i <=0                      = tree
dropJ i jl@(Single size' y) | i == kl     = Empty
    where kl = getSize $ size size'
dropJ i jl@(Append b l1 l2)
    | i < kl                              = Append b (dropJ (i-1) l1) l2
    | i > kl                              = Append b l1 (dropJ (i-1) l2)
    where kl = getSize $ size $ tag jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                             = Empty
takeJ i tree | i <=0                      = tree
takeJ i jl@(Single size' y) | i == kl     = jl
    where kl = getSize $ size size'
takeJ i jl@(Append b l1 l2) 
    | i < kl                              = Append b (takeJ (i-1) l1) l2
    | i < kl                              = Append b l1 (takeJ (i-1) l2)
    where kl = getSize $ size $ tag jl

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _          = Nothing
_      !!? i | i < 0  = Nothing
(x:xs) !!? 0          = Just x
(x:xs) !!? i          = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- testJL :: JoinList Size Char
-- testJL = 
--     Append (Size 210) 
--         (Append (Size 30) 
--             (Single (Size 5) 'y') 
--             (Append (Size 6) 
--                 (Single (Size 2) 'e') 
--                 (Single (Size 3) 'a'))) 
--         (Single (Size 7) 'h')