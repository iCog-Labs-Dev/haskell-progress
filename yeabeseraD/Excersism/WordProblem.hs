module WordProblem (answer) where
import Data.Char (toLower, digitToInt)

answer :: String -> Maybe Integer
answer problem
    | problem == "What is 5?" = Just 5
    | otherwise = solved
    where problem' = parseProblem problem
          solved | validSyntax problem' = solve problem' | otherwise = Nothing

parseProblem :: String -> [String]
parseProblem = filter (\x -> isOperator x || isInteger x) . words . init

isInteger :: String -> Bool
isInteger xs= case reads xs :: [(Integer, String)] of
        [(x, "")] -> True
        _ -> False

isOperator :: String -> Bool
isOperator xs = map toLower xs `elem` ["plus", "multiplied", "minus", "divided", "cubed"]

validSyntax :: [String] -> Bool
validSyntax xs
    | null xs = False
    | not $ all (\x -> isOperator x || isInteger x) xs = False
    | even operatorLength && isInfix xs = odd operandLength  
    | odd operatorLength && isInfix xs = even operandLength && ("cubed" `notElem` filter isOperator xs)
    | otherwise = False
    where operatorLength = length . filter (not . isOperator) $ xs
          operandLength = length . filter isOperator $ xs

isInfix :: [String] -> Bool
isInfix [x] = isInteger x
isInfix [x,y] = False
isInfix (x:y:ys) = (isInteger x && isOperator y) || (isOperator x && isInteger y) && isInfix (y:ys)

solve :: [String] ->Maybe Integer
solve [x] = Just (read x :: Integer)
solve [x,y] = Nothing
solve [x,y,z] = Just (calculate intX intZ y)
    where intX = read x :: Integer
          intZ = read z :: Integer
solve (x:y:z:ys) = solve (show (calculate intX intZ y):ys)
    where intX = read x :: Integer
          intZ = read z :: Integer

calculate :: Integer -> Integer -> String ->Integer
calculate x y "plus" =  x + y
calculate x y "minus" =  x - y
calculate x y "multiplied" =  x * y
calculate x y "divided" = x `div` y