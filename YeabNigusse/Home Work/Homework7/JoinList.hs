import Sized


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                                                deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty jl1 = jl1
(+++) jl2 Empty = jl2
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x y) = x
tag (Append x jl1 jl2) = x

jlbToList :: Monoid m => JoinList m a -> [a]
jlbToList Empty = []
jlbToList  (Single m a) = [a]
jlbToList  (Append m l1 l2) = jlbToList l1 ++ jlbToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl = jlbToList jl !!? n 

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


-- dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- dropJ = drop n (jlToList jl)

-- jlToList (dropJ n jl) == drop n (jlToList jl)

-- The formal definition of a monoid is an algebraic structure which has an 
-- associative binary operation over a set with an identity element.
-- BINERY OPERATION- combine two elements with closed property

-- EXAMPLE

-- instance Monoid String where
--     mempty :: String
--     mempty = ""
--     mappend :: String -> String -> String
--     mappend a b = a ++ b
