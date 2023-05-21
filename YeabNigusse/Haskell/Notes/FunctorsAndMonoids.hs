{--
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
--}
main :: IO ()
main = do 
        putStrLn "what is your name please?" --functor with IO
        line <- fmap ("Hello "++) getLine
        putStrLn line

-- instance Functor ((->) r) where
-- fmap f g =(\x -> f (g x))

-- instance Functor Maybe where             instance of Maybe for functor typeclass
--         fmap f (Just x) = Just (f x)  
--         fmap f Nothing = Nothing  

data CMaybe a = CNothing | CJust Int a deriving (Show)  

instance Functor CMaybe where
    fmap f CNothing = CNothing -- this is doesn't follow the rule of or laws of Functors since according to first law of 
                               -- functor maping id over a functor should give the functor itself
    fmap f (CJust counter x) = CJust (counter+1) (f x)
{--instance Applicative [] where
        pure x = [x]
        fs <*> xs = [f x | f <- fs, x <- xs]--}

-- APPLICATIVE FUNCTORS OVER LIST

--[x*y| x<- [1,2,3], y<- [1,2,3]] cna be done with list comprhension
--[1,2,3,2,4,6,3,6,9]

--(*) <$> [1,2,3] <*> [1,2,3] can be done with applicative functors

{--
instance  applicative IO where
        pure = return
        a <*> b = do
                f <- a
                x <- b
                return (f x)
--}
-- Example
myAction :: IO String
myAction = (++) <$> getLine <*> getLine

--Monoids



