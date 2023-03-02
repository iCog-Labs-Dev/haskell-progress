import qualified Data.Map as Map

-- LET US WORK ON OUR OWN TYPECLASSES

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- in the above code Shape is our data type whereas Circle and Rectangle are called value constractors  

-- LET US DO SOMETHING WITH OUR DATA TYPE

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * abs (y2 - y1)

-- REDEFINING SHAPE DATA TYPE
data Point = Point Float Float
data Shape' = Circle' Point Float | Rectangle' Point Point

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r^2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * abs (y2 - y1)

-- LET US DO A STUDENT DATA TYPE 
data Student = Student String String Int String deriving (Show)
-- guy = Student "Yeab" "Nigusse" 21 "computer science" (Not descriptive!)

-- let us make it descriptive
firstName :: Student -> String
firstName (Student firstName _ _ _ ) = firstName
-- firstName guy (to get in describtive way)
lastName :: Student -> String
lastName (Student _ lastName _ _ ) = lastName

id :: Student -> Int
id (Student _ _ id _) = id

department :: Student -> String
department (Student _ _ _ department) = department
-- it is tedious to do all this for each attributs(value constructers)
-- Let us do it in simpler way 

{--data Book = Book {
    title :: String,
    auter :: String,
    pages :: Int
}deriving (Show)--}
-- Book {title = "wsane", auter = "eyob mamo", pages = 120}
-- that is more descriptive and shortest way to write

--tellBook :: Book -> String
--tellBook (Book {title = "wsane", auter = "Dr Eyob Mamo", pages = 120}) = "This Book is Called " ++ title ++ ", Written By " ++ auter ++ " Containing " ++ show pages 
-- trying to use Book parameters

-- TYPE PARAMETERS
data Boo a b c = Boo {
                tit :: a,
                aut :: b,
                num :: c
}deriving(Show)
--tellBook :: Boo -> String
tellBook :: Show a => Boo [Char] [Char] a -> [Char]
tellBook (Boo {tit = a, aut = b, num = c}) = "This Book is Called " ++ a ++ ", Written By " ++ b ++ " Containing " ++ show c 

data Car a b c = Car { company :: a  
, model :: b  
, year :: c   
} deriving (Show)  

--tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- THE ABOVE CODE IS NOT WORKING, I DON'T KNOW WHY

-- LET US PRACTICE TYPE PARAMETERS

data Vector a = Vector a a a deriving(Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+j) (j+m) (k+n)

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector l m n) = Vector (i*j) (j*m) (k*n)

scalarMult :: (Num a ) => Vector a -> Vector a -> a
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

--Deriving Instances 

data Person = Person {firstname :: String,
                      lastName' :: String,
                      age' :: Int}deriving(Eq)
--let mikeD = Person {firstname = "Yeab", lastName' = "Nigusse", age' = 21}
--let yeab = Person {firstname = "Yeab", lastName' = "Nigusse", age' = 22}
--mikeD == yeab

data Day = Monday | Tusday | Wedsday | Thursday | Friday | Saturday | Sunday deriving(Eq, Ord, Show, Read, Enum, Bounded)

{--Monday == Tusday
False
Monday `compare` Tusday
LT
ghci> Monday
Monday
ghci> read "Monday" :: Day
Monday
ghci> maxBound :: Day
Sunday
ghci> minBound :: Day
Monday
ghci> succ Monday
Tusday
ghci> pred Tusday 
Monday--}

-- TYPE SYNONYMS

data DormState = Taken | Free deriving(Show, Eq)

type Code = String

type DormMap = Map.Map Int (DormState, Code)

dormLookUp :: Int -> DormMap -> Either String Code
dormLookUp dormNumber map = case Map.lookup dormNumber map of  
                            Nothing -> Left $ "Dorm Number " ++ show dormNumber ++ " Doesn't Exist!"
                            Just(state, code) -> if state /= Taken then Right code 
                            else Left $ "Locker " ++ show dormNumber ++ " is alredy taken!"
dorms :: DormMap  
dorms = Map.fromList   
                    [(100,(Taken,"ZD39I"))  
                    ,(101,(Free,"JAH3I"))  
                    ,(103,(Free,"IQSA9"))  
                    ,(105,(Free,"QOTSA"))  
                    ,(109,(Taken,"893JJ"))  
                    ,(110,(Taken,"99292"))]  

-- RECURSIVE DATA STRUCTURE
-- LET US IMPLEMENT BINERY SEARCH TREE

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show, Read, Eq)

singlton :: (Ord a) => a -> Tree a -> Tree a
singlton x EmptyTree = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singlton x EmptyTree
insertTree x (Node a left right)
           | x == a = Node x left right
           | x < a = Node a (insertTree x left) right 
           | x > a =  Node a left (insertTree x right)

findElem :: (Ord a) => a -> Tree a -> Bool
findElem x EmptyTree = False
findElem x (Node a left right)
           | x == a = True
           | x < a = findElem x left
           | x > a = findElem x right

{--ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> let numsTree = foldr insertTree  EmptyTree nums
ghci> numsTree
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
ghci> 8 `findElem` numsTree
True--}

-- TYPECLASSES 102
-- let us make our own typeclass to be an instance of another typeclass by implementing their function

data TraficLight = Red | Yellow | Green

instance Eq TraficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TraficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green =  "Green light"
--ghci> [Red, Yellow, Green]
--[Red light,Yellow light,Green light]

-- A YES-NO TYPECLASSES
