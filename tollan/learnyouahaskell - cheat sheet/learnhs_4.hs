-- ******** Syntax in Functions
-- ****** Pattern Matching
match123 :: (Integral a) => a -> String  -- define its type
match123 1 = "One!"  -- return "One" if the paramater matches '1'
match123 2 = "Two!"  -- return "Two" if the param doesn't match '1', but matches '2'
match123 3 = "Three!"  
match123 x = "Not between 1 and 3"  -- return "Not between ..." if it matches any other Integral (catch-all pattern)

-- ** factorial example with pattern matching
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) -- (catch-all pattern)

-- ** If we didn't have a catch-all pattern, it returns an error when we give it parameters not included in the pattern
noCatchAll :: Char -> String
noCatchAll 'a' = "Albert"
noCatchAll 'b' = "Bob"
-- if we run noCatchAll 'g', it would return an error

-- ** Adding two vectors with and without pattern matching
addVectorsWithout :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectorsWithout a b = (fst a + fst b, snd a + snd b) -- this only works for tuples with two elements
-- We could use pattern matching instead of the 'fst' and 'snd' and also add a 'third' for triples
first :: (a, b, c) -> a  
first (x, _, _) = x  
second :: (a, b, c) -> b  
second (_, y, _) = y
third :: (a, b, c) -> c  
third (_, _, z) = z   

-- ** Pattern matching in lists
listPattern = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
sumListTuples = [a+b | (a,b) <- listPattern]  -- [4,7,6,8,11,4]   

-- **  A pattern like 'x:xs' will bind the head of the list to 'x' and the rest of it to 'xs'.
-- Patterns that have : in them only match against lists of length 1 or more.
getHead' :: [a] -> a  
getHead' [] = error "Can't call head on an empty list"  
getHead' (x:_) = x  -- the list to the right of ':' doesn't matter

-- ** We can be more specific when telling the patterns of lists
matchLists :: (Show a) => [a] -> String  
matchLists [] = "The list is empty"  
matchLists (x:[]) = "The list has one element: " ++ show x  -- we can write (x:[]) as [x]
matchLists (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  -- we can write (x:y:[]) as [x,y]
matchLists (x:y:_) = " The list has more than two elements: " ++ show x ++ " and " ++ show y -- This can also match with two elements

-- ** We can find out the length of a list recursively using pattern matching
lengthOfList :: (Num b) => [a] -> b  
lengthOfList [] = 0  
lengthOfList (_:xs) = 1 + lengthOfList xs  -- ignore the head and apply recursively for the tail

-- ** Sum of list using resursion and pattern matching
sumOfList :: (Num a) => [a] -> a
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

-- ** You can get the whole parameter and its pattern using the '@' symbol
firstLetterOf :: String -> String  
firstLetterOf "" = "Empty string, whoops!"  
firstLetterOf all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

-- ************* Guards *********************
-- Guards are like if else trees and they really work with patterns
-- ** Using guardto comment on BMI
bmiLabel :: (RealFloat a) => a -> String -- The type can't be fractional because <= 18 might be a real no.
bmiLabel bmi
    | bmi <= 18 = "Underweight" -- Bool = "..."
    | bmi <= 25 = "Normal"
    | bmi <= 30 = "Overweight"
    | otherwise = "Obese" -- otherwise just means True and is the 'default' condition
-- we can also calculate BMI directly (bmi = weight / height ^2)
bmiCalculate :: (RealFloat a) => a -> a -> String 
bmiCalculate weight height
    | weight / height ^ 2 <= 18 = "Underweight" -- ... and so on
-- Guards can also be written on the same line
maxNum :: (Ord a) => a -> a -> a
maxNum a b | a > b = a | otherwise = b

-- ** We can also define and use functions as an infix with backticks
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT | a == b = EQ | otherwise = LT
compareExample = 12 `myCompare` 34

-- ******* 'where'
-- ** Instead of repeating the same calculation many times, we can use the where keyword like this
calculateBmi :: (RealFloat a) => a -> a -> String 
calculateBmi weight height
    | bmi <= 18 = "Underweight"
    | bmi <= 25 = "Normal" -- and so on
    where bmi = weight / height ^ 2 -- must be indented
          normal = 25 -- we can define multiple expressions under 'where' but it is giving me an error for some reason
          (skinny, fat) = (18, 30) -- we can also do pattern matching like this
        -- Here, 'bmi', 'normal', 'skinny' and 'fat' is accessible throughout the guard / function
-- ** Input firstname and lastname and return the initials
getInitials :: String -> String -> String  
getInitials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    
-- ** Input a list of weight and height pairs and return a list of BMIs
calculateBmis :: (RealFloat a) => [(a, a)] -> [a]  
calculateBmis xs = [calcBmi w h | (w, h) <- xs]  
    where calcBmi weight height = weight / height ^ 2  -- we are defining a function that takes parameters 'weight' and 'height'

-- ***** 'let' <bindings> 'in' <expression>
-- As opposed to 'where' binding, 'let' bindings are local and don't span across guards. 
-- They're only accessible to the expression after the 'in' part
areaOfCylinder :: (RealFloat a) => a -> a -> a  
areaOfCylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
-- The main difference between 'where' and 'let' is that 'let' bindings are expressions themselves, 
-- but 'where' bindings are just syntactic constructs. For example:
letExample1 = 5 * (let a = 9 in a + 1) + 2 -- This gives 52, because the let binding returns 10
-- 'let' can also be used to introduce functions in a local scope:
letExample2 = let square x = x * x in (square 5, square 3, square 2)
--  we can bind to several variables inline by separating them with semicolons
letExample3 = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)   
-- we can also do pattern matchnig with 'let'
letExample4 = (let (a,b,c) = (1,2,3) in a+b+c) * 100  
-- we can also use 'let' in list comprehension
-- The names defined in 'let' of a list comprehension are visible to the output function (before the '|')
calculateBmisLet :: (RealFloat a) => [(a,a)] -> [a]
calculateBmisLet weightHeightPair = [bmi | (w,h) <- weightHeightPair, let bmi = w / h ^ 2, bmi > 10] -- we can also add another condition to the predicate
-- We can't use 'bmi' in the '(w, h) <- weightHeightPair' part because it's defined prior to the let binding.
-- We omitted the 'in' part because the visibility of the bindings is already predefined. If we used 'in' in the predicate, it will only be visible to that predicate 

-- ******** Case Expressions
--case <expression> of <pattern> -> <result>  
--                     <pattern> -> <result> ...
-- This is one way of doing this without case expressions
getHeadNoCaseExp :: [a] -> a  
getHeadNoCaseExp [] = error "No head for empty lists!"  
getHeadNoCaseExp (x:_) = x  
-- Here we are using case expressions
getHeadCaseExp :: [a] -> a  
getHeadCaseExp xs = case xs of [] -> error "No head for empty lists!"  
                               (x:_) -> x  
-- Whereas pattern matching on function parameters can only be done when defining functions,
-- case expressions can be used pretty much anywhere
describeList1 :: [a] -> String  
describeList1 xs = "The list is " ++ case xs of [] -> "empty."  
                                                [x] -> "a singleton list."   
                                                xs -> "a longer list."  
 
describeList2 :: [a] -> String  
describeList2 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  