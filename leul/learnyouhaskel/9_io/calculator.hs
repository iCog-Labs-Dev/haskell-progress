import Control.Monad ( when )
main = do
  putStrLn "Enter the first number"
  a <- getLine
  putStrLn "Enter the second number"
  b <- getLine

  putStrLn "Select operation [+ - * /] [q to quit]"
  op <- getLine

  let num1 = read a :: Float
      num2 = read b :: Float

  case op of
    "+" -> putStrLn $ "The sum is = " ++ show (num1 + num2)
    "-" -> putStrLn $ "The difference is = " ++ show (num1 - num2)
    "*" -> putStrLn $ "The product is = " ++ show (num1 * num2)
    "/" -> putStrLn $ "The quotient is = " ++ show (num1 / num2)
    "q" -> putStrLn "Bye!"
    x -> putStrLn "Invalid input."

  when (op /= "q")  do main