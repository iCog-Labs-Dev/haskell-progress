module IOFunctions(failure, help, modes) where

import Data.List (intersperse)
import RPN ( solveRpnM, toRpn )

modes :: [(String, [String]-> IO ())]
modes = [
        ("--rpn", rpnMode),
        ("--add", normalMode "+"),
        ("--sub", normalMode  "-"),
        ("--mult", normalMode "*"),
        ("--div", normalMode "/"),
        ("--help", help)
        ]

rpnMode :: [String] -> IO ()
rpnMode expression = do
    let rpn = toRpn (unwords expression)
        solved = case solveRpnM rpn of
            Nothing -> "Invalid expression input"
            (Just result) -> unwords expression ++ " = " ++ show result
    putStrLn solved

normalMode ::String -> [String] -> IO()
normalMode opp [numStr1, numStr2] = do
    let solution = case sum of
                    Nothing -> "Invalid input"
                    (Just x) -> numStr1 ++ ' ':opp ++ ' ':numStr2 ++ " = "++show x
    putStrLn solution
    where sum = binaryOpp opp (read numStr1)  (read numStr2)
normalMode opp [] = putStrLn "Please Make sure you entered two operands"
normalMode _ _ = putStrLn "Invalid input"

binaryOpp :: (Fractional a, Eq a) => String -> a -> a -> Maybe a
binaryOpp opp num1 num2 = case opp of
        "+" -> return (num1 + num2)
        "-" -> return (num1 - num2)
        "/" -> validDivision
        "*" -> return (num1 * num2)
    where validDivision = if num2 == 0 then Nothing else Just $  num1 / num2

failure ::[String] -> IO ()
failure _ = do
        
    putStrLn "Invalid arguement detected. Check the help for more information\n"
    help []

help ::[String] -> IO ()
help _ = do
    let helpStr = "Help menu :\n"++
                  " | --add    <firstNumber>  <secondNumber> : To add two numbers\n"++
                  " | --sub    <firstNumber>  <secondNumber> : To subtract two numbers\n" ++
                  " | --mult   <firstNumber>  <secondNumber> : To mutliply two numbers\n" ++
                  " | --div    <firstNumber>  <secondNumber> : To divide two numbers\n"++
                  " | --rpn    <expresion>                   : To perform mulitple operations at the same time.\n"++
                  "\t\t\t\t\t    Use brackets to imply prefered preference and surround the expression with double coutes\n"++
                  " | --help                                 : To see the list of available commands\n"
    putStrLn helpStr