-- 1. Give another possible calculation for the result of double (double 2).
double x = x+x
quadreble x = double x + double x

-- 2. Show that sum [x] “ x for any number x.
{--
 sum [] = 0
 sum[x] = x + sum [] = x + 0 which is an identity property
-}
--3. Define a function product that produces the product of a list of numbers,
--and show using your definition that product [2,3,4] “ 24.

product' [] = 1
product' (x:xs) = x * product' xs

-- How should the definition of the function qsort be modified so that it 
-- produces a reverse sorted version of a list

reverseQuickSort [] = []
reverseQuickSort (x:xs)= let smaller = [a| a <- xs, a < x]
                             bigger =  [a| a <- xs, a > x]
                         in reverseQuickSort bigger ++ [x] ++ reverseQuickSort smaller

{--
What would be the effect of replacing <= by < in the original definition of
    qsort? Hint: consider the example qsort [2,2,3,1,1].
ANSWERE
 it removes dublicated elements from the sorted list
--}

