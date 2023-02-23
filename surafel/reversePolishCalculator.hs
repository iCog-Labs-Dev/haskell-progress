{-
    A command line program that calculates a reverse polish notaion math problem.
    it does not validate the notaion so try to enter a valid notation.
    this program is not an end product as it produces an error for more complicated
    inputs. it will be updated once solved.
        the error: "Exception: Prelude.tail: empty list"
        example input: "10 6 9 3 + -11 * / * 17 + 5 +"
-}

main = do
  welcome
  cliLoop

welcome = do
  putStrLn "Welcome!!! the reverse polish reverse notaion calculator,\nplease enter a valid polish notaion expression."

cliLoop = do
  putStr "--> "
  inp <- getLine
  print (calculate (words inp) [])
  cliLoop

doOperation :: Char -> Float -> Float -> Float
doOperation '+' x y = x + y
doOperation '-' x y = x - y
doOperation '*' x y = x * y
doOperation '/' x y = x / y

calculate :: [String] -> [Float] -> Float
calculate [] a = head a
calculate inp operands =
  if (head (head inp) `notElem` "+-*/") && (length (head inp) == 1)
    then calculate (tail inp) ((read (head inp) :: Float) : operands)
    else calculate (tail inp) (result : tail (tail operands))
  where
    result =
      doOperation
        (head $ head inp)
        (head (tail operands))
        (head operands)

-- 10 6 9 3 + -11 * / * 17 + 5 +

{-

6 9 3 + -11 * / * 17 + 5 +    [10]
9 3 + -11 * / * 17 + 5 +      [6, 10]
3 + -11 * / * 17 + 5 +        [9, 6, 10]
+ -11 * / * 17 + 5 +          [3, 9, 6, 10]
-11 * / * 17 + 5 +            [12, 6, 10]          do 9 + 3 = 12
\* / * 17 + 5 +               [-11, 12, 6, 10]
/ * 17 + 5 +                  [-132, 6, 10]        do 12 * -11 = -132
\* 17 + 5 +                   [-0.0454 , 10]       do 6 / -132 = -0.0454
17 + 5 +                      [-0.454]             do -0.0454 * 10 = -0.454
+ 5 +                         [17, -0.454]
5 +                           [16.95]              do 17 + -0.454 = 16.95
+                             [5, 16.95]
[]                            [21.95]              do 5 + 16.95 == 21.95

21.95
-}