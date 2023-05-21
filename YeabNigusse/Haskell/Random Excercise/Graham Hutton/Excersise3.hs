-- 1. What are the types of the following values?

leters = ['a', 'b', 'c'] -- [char]
triple = ('a', 'b', 'c') -- (char, char, char)
list = [(False,'o'),(True,'1')] -- [(Bool, char)]
list2 = ([False,True],['0','1']) -- ([Bool], [Char])
builtins = [tail, init, reverse] -- [[a] -> [a]]

-- 2. Write down definitions that have the following types; it does not matter what
--the definitions actually do as long as they are type correct.

bools :: [Bool]
bools = map even [1 .. 10]

nums :: [[Int]]
nums = map row  [1 .. 5]
           where 
            row n = map colum [1, 2]
                 where colum m = n*m-- creating 5 by 2 matrix using map function

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

-- 3. What are the types of the following functions?

second xs = head (tail xs) -- [a] -> a
swap (x,y) = (y,x) -- (a,b) -> (b,a)
pair x y = (x,y) -- (a -> b -> (a, b)
double x = x*2 -- Num a => a -> a
palindrome xs = reverse xs == xs -- Eq a => [a] -> Bool
twice f x = f (f x) -- (a -> a) -> a -> a

