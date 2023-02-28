operations :: [String]
operations = ["+", "-", "*", "/"]


performOperation :: (Fractional a) => a -> a -> String -> a
performOperation a b op = case op of "+" -> a + b
                                     "-" -> a - b
                                     "*" -> a * b
                                     "/" -> a / b


calculator :: [String] -> [Float]
calculator [] = []
calculator [x] = [read x]
calculator xs = calculate xs []


calculate :: [String] -> [Float] -> [Float]
calculate [op] (op1:op2:_) = [performOperation op1 op2 op]
calculate xs stack
    | current `elem` operations = calculate (tail xs) ((let a = stack !! 1
                                                            b = head stack
                                                       in performOperation a b current) : stack)
    | otherwise = calculate (tail xs) (read current : stack)
    where current = head xs


main :: IO ()
main = do 
    putStrLn "Enter a reverse polish expression to be evaluated"
    putStrLn "For example: 4 3 + 4 *"
    line <- getLine
    print $ head $ calculator $ words line
    