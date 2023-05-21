import System.Environment (getArgs)


interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
        args <- getArgs
              case args of
                  [input,output] -> interactWith function input output
                  _ -> putStrLn "error: exactly two arguments needed"

-- replace "id" with the name of our function below
myFunction = id


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail lst
          | length lst == 1 = Just []
          | otherwise = Just (tail lst)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast lst = Just (last lst)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit lst 
          | length lst == 1 = Just []
          | otherwise = Just (init lst)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p lst = filter p lst : filter (not . p) lst : []

asInt_fold :: String -> Int
asInt_fold str = _


