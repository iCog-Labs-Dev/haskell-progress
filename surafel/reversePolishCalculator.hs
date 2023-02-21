{-
    A command line program that calculates a reverse polish notaion math problem.
    note that it does not validate the notaion so try to enter a valid notation.
-}

main = do
  begin
  cliLoop

begin = do
  putStrLn "Welcome!!! the reverse polish reverse notaion calculator,\nplease enter a valid polish notaion expression."

cliLoop = do
  putStr "--> "
  inp <- getLine
  print (inputLoop (words inp) [])
  cliLoop

doOperation :: Char -> Float -> Float -> Float
doOperation '+' x y = x + y
doOperation '-' x y = x - y
doOperation '*' x y = x * y
doOperation '/' x y = x / y

-- parseInp :: String -> Maybe Int

inputLoop :: [String] -> [Float] -> Float
-- inputLoop inp [] =
--     inputLoop inp
inputLoop [] a = head a
inputLoop inp operands =
  -- if the head is integer call lup with arguments # (tail of inp) head:[prev]#
  -- else if it is an operator do operation and call lup with arguments # (tail of inp) answer:[prev]#
  -- 4 13 5 / +
  if head (head inp) `notElem` "+-*/"
    then inputLoop (tail inp) ((read (head inp) :: Float) : operands)
    else inputLoop (tail inp) (result : tail (tail operands))
  where
    result =
      doOperation
        (head $ head inp)
        (head (tail operands))
        (head operands)