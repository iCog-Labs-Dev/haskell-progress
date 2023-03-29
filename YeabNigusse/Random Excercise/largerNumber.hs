-- Obtain two numbers from the keyboard, and determine and display 
-- which (if either) is the larger of the two numbers.

main = do
    putStrLn "Enter the first Number"
    num1 <- getLine
    putStrLn "Enter the second Number"
    num2 <- getLine
    putStrLn (compareNumber (read num1) (read num2))

compareNumber :: Integer -> Integer -> String
compareNumber num1 num2 
            | num1 > num2 = show num1 ++ " is greater than " ++ show num2
            | num1 < num2 = show num2 ++ " is greater than " ++ show num1
            | otherwise = "They are equal"

    
