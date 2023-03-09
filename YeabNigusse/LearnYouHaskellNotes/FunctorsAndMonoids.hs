{--
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
--}
main = do 
        putStrLn "what is your name please?"
        line <- fmap ("Hello "++) getLine
        putStrLn line
-- instance Functor ((->) r) where
--     fmap f g =(\x -> f (g x))

