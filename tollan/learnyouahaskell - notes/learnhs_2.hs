-- ***** Baby's first functions
doubleMe x = x + x
doubleUs x y = (x*2, y*2)
doubleAndAdd x y = x*2 + y*2

-- ****** Conditionals
doubleSmallNo x = if x < 10 
    then x*2 
    else x 
doubleSmallNoAndAdd1 x = 1 + (if x<10 then x*2 else x)

-- ****** Lists
hello = "hello" -- ghci> let hello = "hello"
-- Note that: "hello" == ['h','e','l','l','o']
helloWorld = "hello" ++ "world"
concatList = [1,2,3] ++ [4] -- this way has more overhead because it has to go through the whole first list
lessOverheadConcat x = x:[2,3,4]
-- Note that: ++ is used to concatenate two lists whereas : is used to concatenate a list with a list/character/number
getElementOfIndex x = [12,32,53,14] !! x
multidimensionalList = [[1,2],[4,5]] -- [[1,2], ['a','b']] is not allowed
-- add1 x = x + 1 in add1 5
compareLists = [2] > [1,2,3] && [3,2,1] > [1,100,100] && [1,2] == [1,2]
getVal = [1,2,3,4,5]
getValuesOfLists = [head getVal] : [init getVal]
getValuesOfLists2 = [tail getVal]
reverseList = reverse [5,4,3,2,1]
takeFromList = take 3 [1,2,3,4,5,6] -- takes the first 'n' elements from the list
dropFromList = drop 3 [1,2,3,4,5,6] -- drops the first 'n' elements from the list
sumList = sum [1,2,3] -- theres also 'product [1,2,3]'
inList = 4 `elem` [1,2,3,4] -- returns True if the element exists, otherwise it returns False

-- ********** Ranges
oneToHundred = [1..100]
hundredToOne = [100,99..1]
evenNumbers = [2,4..20]
reverseEvenNumbers = [20,18..2]
alphabetRange = ['a'..'z']
floatingRange = [0.1,0.3..1]  -- generally not recommended to using floating point numbers in ranges
-- Note that: You can use ranges to make infinite lists by not specifying an upper limit. e.g., [1,2..]
takeSomeFromList = take 20 [1,9..] -- it won't go on forever anymore
repeatList = repeat 5 -- [5,5,5,5,5, ... ]
cycleList = cycle [1,2,3] -- [1,2,3,1,2,3,1,2,3 ... ]
takeFromCycle = take 12 (cycle "lol ")

-- ********** List comprehension (Like sets in Math)
simpleListComprehension = [x*2 | x <- [1..10]]
conditionalComprehension = [x*2 | x <- [1..10], x*2 >= 12]
conditionalComprehension2 = [x | x <- [50..100], x `mod` 7 == 3]
evenBoomOddBang = [if x < 10 then "BOOM!" else "Bang" | x <- [1..15], odd x] -- even <10 = "Boom", odd >10 = "Bang"
excludeSomeNumbers = [x | x <- [1..20], x/=9, x/=13, x/=17]
nouns = ["hobo","frog"]  
adjectives = ["lazy","grouchy"]  
listCombinations = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]  -- returns all possibilities of the combination of the lists
removeNonUppercase = [c | c <- "How Are You", c `elem` ['A'..'Z']]
longNestedList = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
evenNumbersInNestedList = [ [ x | x <- xs, even x ] | xs <- longNestedList ]

-- ********* Tuples
-- We use tuples when we know how many components our data should have.
-- Tuples not homogenous (in data type). e.g., (1, "One"), but each different tuple is its own type (1,2) /= (1)
-- (123, "Main St", 2341) - is called a triple. There are no singleton tuples
-- doing this to represent 2D vectors: [[1,2],[3,4],[5,6]] is possible but using a tuple is better because 
-- this won't give an error: [[1,2], [3,4,5]] but this will: [(1,2), (3,4,5)]. We should use 2 numbers to represent 2D vectors
firstFromTuple = fst (12,34) -- you can also use 'snd' to get second element. This won't work on 4-tuples, 5-tuples,...
combineListsToTuples = zip [1..3] ["One", "two", "three"] -- if one list is longer, it just zips with the shorter one
-- Heres how we find out all right angled triangles whose perimeter is 24 and whose each side lengths are <10
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
-- We can find the length of a list using List Comprehension
lengthOfList xs = sum [1 | _ <- xs]