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
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)