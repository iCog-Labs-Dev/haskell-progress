module RPN(solveRpnM, toRpn, solveRpn) where 

import Data.Char (isDigit)
import Control.Monad (liftM, foldM)


solveRpn :: String -> Float
solveRpn = head . foldl calcRPN [] . words

solveRpnM :: String -> Maybe Double
solveRpnM st = do
    [result] <- foldM calcRpnM [] (words st)
    return result

toRpn :: String -> String
toRpn xs = filter (`notElem` "()") rpnWithBrackets
    where rpnWithBrackets = reverse $ pushToStack xs "" ""

calcRPN :: [Float] -> String -> [Float]
calcRPN (x:y:ys) "+" = x + y : ys
calcRPN (x:y:ys) "-" = y - x : ys
calcRPN (x:y:ys) "*" = x * y : ys
calcRPN (x:y:ys) "/" = y / x : ys
calcRPN xs numberString = read numberString : xs

calcRpnM :: [Double] -> String -> Maybe [Double]
calcRpnM (x:y:ys) "+" = return (x + y : ys)
calcRpnM (x:y:ys) "-" = return (y - x : ys)
calcRpnM (x:y:ys) "*" = return (x * y : ys)
calcRpnM (x:y:ys) "/" = return (y / x : ys)
calcRpnM xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: String -> Maybe Double
readMaybe st = 
    case reads st of [(x,"")] -> return x
                     _ -> Nothing

pushToStack :: String -> String -> String -> String
pushToStack [] expression stack = popAllStack expression stack
pushToStack (x:xs) expression stack 
    | isDigit x = pushToStack restExpr (' ':(reverse . show) digit ++ expression) stack
    | x == ')' = pushToStack xs newExpr newStack 
    | null stack = pushToStack xs expression (x:stack)
    | precedence (head stack) >= precedence x = pushToStack xs (' ':head stack: expression) (x: tail stack)
    | otherwise = pushToStack xs expression (x:stack)
    where (newExpr, newStack) = popUntilOpenBracket expression stack
          [(digit, restExpr)] = reads (x:xs) :: [(Integer, String)]

popAllStack :: String -> String -> String
popAllStack expr [] = expr
popAllStack expr [x] = ' ':x:expr
popAllStack expr (x:ys) = popAllStack (' ':x:expr) ys

popUntilOpenBracket :: String -> String -> (String, String)
popUntilOpenBracket expr [] = (expr, "")
popUntilOpenBracket expr ['('] = (expr, "")
popUntilOpenBracket expr ('(':xs) = (expr,xs)
popUntilOpenBracket expr (x:ys) = popUntilOpenBracket (' ':x:expr) ys

precedence :: Char -> Int
precedence '+' = 2
precedence '*' = 3
precedence '/' = 3
precedence '-' = 2
precedence '(' = 5
precedence ')' = 0
precedence xs = 1