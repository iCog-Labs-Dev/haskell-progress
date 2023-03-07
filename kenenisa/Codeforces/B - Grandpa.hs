-- B. Grandpa
-- time limit per test 1 second
-- memory limit per test 256 megabytes
-- input standard input
-- output standard output
-- Biruk's grandpa has a large collection of special stones. There is a legend that says one who has at least 5 unique special stones will be granted a wish by a higher power. Biruk's grandpa is sick. Help Biruk find out if his grandpa can be granted a wish.

-- Input
-- The first and only line of input contains the stones in grandpa's collection, where each stone is a lowercase latin letter ('a' to 'z').

-- Output
-- Print 'YES' if grandpa can be granted a wish. Otherwise, print NO.

-- Examples

-- input
-- a x a c d d
-- output
-- NO

-- input
-- d f g h f d s
-- output
-- YES

count :: String -> String -> String
count reserve []
  | length reserve == 5 = "YES"
  | otherwise = "NO"
count reserve (x : xs)
  | length reserve == 5 = "YES"
  | x `notElem` reserve = count (x : reserve) xs
  | otherwise = count reserve xs
 
main :: IO ()
main = do
  a <- getLine
  putStrLn (count [] (map head (words a)))
