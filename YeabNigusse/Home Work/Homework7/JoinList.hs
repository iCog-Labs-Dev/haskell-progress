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

-- The formal definition of a monoid is an algebraic structure which has an 
-- associative binary operation over a set with an identity element.
-- BINERY OPERATION- combine two elements with closed property

-- EXAMPLE

-- instance Monoid String where
--     mempty :: String
--     mempty = ""
--     mappend :: String -> String -> String
--     mappend a b = a ++ b
