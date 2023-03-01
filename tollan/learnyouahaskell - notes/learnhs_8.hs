-- ******** Creating your own type and typeclasses
import qualified Data.Map as Map

-- ** We can define new datatypes using the data keyword
-- ** Value constructors are  functions that ultimately return a value of a data type
-- ** The 'Circle' value constructor has three fields/parameters, 'Rectangle' has four
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- ** Let's make a function that takes a shape and returns its area.
area :: Shape -> Float -- we can't do Circle -> Float because circle is not a type, Shape is
area (Circle _ _ r) = pi * r^2 -- pattern matching against value constructors
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 

areaCircle = area $ Circle 10 10 20
areaRectangle = area $ Rectangle 0 0 10 10
--  if we try to just print out Circle 10 20 5 in the prompt, we'll get an error.
-- That's because Haskell doesn't know how to display our data type as a string
data Shape1 = Circle1 Float Float Float deriving (Show) -- Now we can do Circle1 10 10 10 in ghci ... Circle 10.0 10.0 10.0
-- Value constructors are functions
sameCoordCircles = map (Circle1 10 15) [12, 23, 34] -- [Circle1 10.0 15.0 12.0, Circle1 10.0 15.0 23.0, Circle1 10.0 15.0 34.0]

-- ** Let's create another datatype to make our shape more understandable
data Point = Point Float Float deriving (Show)  -- it's common to use the same name for the data type and value constructor if there is only one valus constructor
data Shape3 = Circle3 Point Float | Rectangle3 Point Point deriving (Show) 
area3 :: Shape3 -> Float
area3 (Circle3 _ r) = pi * r^2
area3 (Rectangle3 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- **  A function that nudges a shape. It takes a shape, the amount to move it on the x axis and y axis
-- and then returns a new shape that has the same dimensions
nudgeShape :: Shape3 -> Float -> Float -> Shape3
nudgeShape (Circle3 (Point x y) r) a b = Circle3 (Point (x+a) (y+b)) r
nudgeShape (Rectangle3 (Point x1 y1) (Point x2 y2)) a b = Rectangle3 (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) 

nudgeExample = nudgeShape (Circle3 (Point 34 34) 10) 5 10 
-- If we don't want to deal directly with points, we can make some auxilliary functions that create shapes of some size
-- at the zero coordinates and then nudge those.
baseCircle :: Float -> Shape3
baseCircle r = Circle3 (Point 0 0) r
baseRect :: Float -> Float -> Shape3
baseRect x y = Rectangle3 (Point 0 0) (Point x y)

myCircle = baseCircle 10
myNudgedCircle = nudgeShape myCircle 12 34

-- ** If we wanted to export the functions and types that we defined here in a module, we can start off like this:  
{- module Shapes   
( Point(..)  
, Shape(..) --  export all the value constructors for Shape  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  -}
-- If you want to export all the value constructors for a given type, just write '..'

-- ****** Record Syntax
-- If we want to define a person (firstname lastname age height),  we can do this
data Person0 = Person0 String String Int Float
-- or we can do this using record syntax
data Person = Person {
    firstname :: String,
    lastname :: String,
    age :: Int,
    height :: Float
} deriving (Show)
-- By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height
-- data Dog = Dog { name :: String, age:: Int}   ....  Error, 'age' already defined
personExample = Person {lastname="Doe", firstname ="Jhon", age=23, height=180.3}

-- ******* Type parameters
-- Just like vallue constructors can take values as parameters to produce new values, Type constructors can take types
-- as parameters to produce new types. E.g., data Maybe' a = Nothing' | Just' a -- Here, 'Maybe' is a type constructor
-- because 'a' is a type parameter. We can't have a type of just Maybe
-- E.g.,
data Car a = Car {
    name :: String,
    model :: String,
    year :: a
} deriving (Show)

myCar = Car {name="Ford", model="Mustang", year=1967} -- myCar = Car "Ford" "Mustang" 1967
myCar2 = Car {name="Ford", model="Mustang", year="Nineteen sixty seven"}

tellCar :: Show a => Car a -> String
tellCar (Car {name=n, model=m, year=y}) = "This " ++ n ++ " " ++ m ++ " was built in " ++ show y

-- *********** Derived Instances
-- We can derive our own datatypes to typeclasses
data PersonEq = PersonEq {
    firstnameEq :: String, lastnameEq :: String, ageEq :: Int
} deriving (Eq) -- All the fields (String, Int) must be a part of the Eq typeclass 
-- Now we can do == and /= operations on it. It will first check if the value constructors match, then it checks the data in each fields

mike = PersonEq "Mike" "Hannigan" 28
tim = PersonEq "Tim" "Cook" 27
compMikeTim = mike == tim -- mike /= tim
compMikeMike = mike == PersonEq "Mike" "Hannigan" 28
-- Now we can use it as the a for all functions that have a class constraint of Eq a in their type signature, such as elem
people = [mike, tim, PersonEq{firstnameEq="Jim", lastnameEq="Bou", ageEq=26}] -- mike `elem` people ... True

-- ** We can add more typeclasses to our datatype (Like Show, Read if we want to convert it to and from Strings)
data PersonEqShowRead = PersonEqShowRead { nameESR :: String, ageESR :: Int } deriving (Eq, Show, Read)
nick = PersonEqShowRead "Nick Nack" 25
showPerson = "This person is: " ++ show nick
readPerson = read "{nameESR = \"Bill Nai\", ageESR = 35}" :: PersonEqShowRead -- We should tell it the datatype explicitly
-- Implicitly: read "{nameESR = \"Bill Nai\", ageESR = 35}" == mike

-- ** If we compare two values of the same type that were made using different constructors,
-- the value which was made with a constructor that's defined first is considered smaller.
-- data Bool' = False' | True' deriving (Ord)
-- compBool = True' `comp` False' -- GT ... True > False
data Maybe' a = Nothing' | Just' a -- .... Just a > Nothing.  We can't do Just (*3) > Just (*2) because functions are not instances of Ord typeclass

-- ** We can easily use algebraic data types to make enumerations and the Enum and Bounded typeclasses help us with that:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
-- Because all the value constructors are nullary (take no parameters/fields), we can make it part of the Enum typeclass
-- We can also make it part of the Bounded typeclass, which is for things that have a lowest and highest possible values
-- Bounded properties
minDayBounded = minBound :: Day -- Monday ... maxBound :: Day will be Sunday
-- Enum properties
succDay = succ Monday -- Tuesday
listDays = [Tuesday .. Friday] -- [Tuesday,Wednesday,Thursday,Friday]


-- ********** Type Synonyms
type String' = [Char]
--phoneBook :: [(String,String)]  
-- phoneBook = [ ("aaron","555-2938"), ("bill","555-5643"), ... ]
-- type PhoneBook = [(String, String)] ... we can create a type synonym that conveys a phonebook
type Name = String
type PhoneNo = String
type PhoneBook = [(Name, PhoneNo)]
-- We can implement a function that takes name & number and sees if that combination is in our phonebook
checkPhonebook :: Name -> PhoneNo -> PhoneBook -> Bool
checkPhonebook name pno pbook = (name, pno) `elem` pbook

--Type synonyms can also be parameterized. If we want a type that represents an association list type
-- but still want it to be general so it can use any type as the keys and values, we can do this:
type AssocList key val = [(key, val)]
assocListExample = [(1,"One"),(2,"Two")] :: AssocList Int String
-- ** Just like finctions, we can partially apply type parameters and get new type constructors from them
-- myDataMap = Map.Map Int String ... general syntax for Data.Map
type IntMap val = Map.Map Int val -- Or we can do this ... type IntMap = Map.Map Int (partially applied type param)

-- ** A data type that takes two types as its parameters is the 'Either a b' type
data Either' a b = Left' a | Right' b deriving(Eq, Ord, Read, Show) -- Two value constructors with different types

-- Lets do this example. Suppose you are giving lockers to students. The student gives you a locker number and you check
-- whether the locker doesn't exist or is taken, if not, you give the student the locker code.

data LockerState = Taken | Free deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNo lockerMap =
    case Map.lookup lockerNo lockerMap of
        Nothing -> Left $ "Locker number "++ show lockerNo ++" doesn't exist"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left "The locker is alredy taken"

-- Lets run the function with an example
myLockerMap = Map.fromList [(1,(Free,"a1b2")), (2,(Taken,"c3d4")), (3,(Free,"e5f6")), (4,(Taken,"g7h8")) ] :: LockerMap
lookup1 = lockerLookup 1 myLockerMap -- Right "a1b2"
lookup2 = lockerLookup 2 myLockerMap -- Left "The locker is alredy taken"
lookup3 = lockerLookup 5 myLockerMap -- Left "Locker number 5 doesn't exist"
-- We could have used a Maybe a to represent the result but then we wouldn't know why we couldn't get the code.

-- ** Recursive data structures
-- E.g., a tree is either an empty tree or it's an element that contains some value and two trees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) -- we call the data type 'Tree' recursively

-- Here's a function that takes a tree and an element and inserts that element into the Tree. We do this by comparing the
-- element to the root node; if it's smaller, we go left, if it's larger, we go right... until we reach an empty tree.

insertToTree :: (Ord a) => a -> Tree a -> Tree a
insertToTree x EmptyTree = Node x EmptyTree EmptyTree
insertToTree x (Node a left right)
    | x < a = Node a (insertToTree x left) right
    | x > a = Node a left (insertToTree x right)

myTree = insertToTree 5 EmptyTree -- Node 5 EmptyTree EmptyTree
myTree2 = insertToTree 3 myTree -- Node 5 (Node 3 EmptyTree EmptyTree) EmptyTree
myTree3 = insertToTree 7 myTree2 -- Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)
myTree4 = insertToTree 1 myTree3 -- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)
myTree5 = insertToTree 8 myTree4 -- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree (Node 8 EmptyTree EmptyTree))

-- We can easily create a tree from a list of values
treeList = [8,6,4,1,7,3,5]
myListTree = foldr insertToTree EmptyTree treeList
myListTree2 = foldl (\acc x -> insertToTree x acc) EmptyTree treeList
myListTree3 = foldr (\x acc -> insertToTree x acc) EmptyTree treeList

--  This function is used to search an element in the tree and return a Bool value
searchTree :: (Ord a, Eq a) => a -> Tree a -> Bool
searchTree x EmptyTree = False
searchTree x (Node a left right)
    | x == a = True
    | x < a = searchTree x left
    | x > a = searchTree x right
 
 -- ** Making your own typeclasses and type instances
{- class Eq' a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  -} 

data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  
showRed = show Red 

-- ** The Yes-No typeclass
-- Let's create a new typeclass with javascript like behavior ... (0, "", false) == False and the rest is True

-- Declaring a new Typeclass
class YesNo a where -- The typeclass can have an instance of type 'a'
    yesno :: a -> Bool -- It always returna a Bool value

-- Defining some instances 
instance YesNo Int where
    yesno 0 = False
    yesno _ = True -- YesNo will be True for all Int except 0
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno = id -- 'id' is just a standard library function that takes a parameter and returns the same thing
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True
-- Now we can use the typeclass we defined
yesNoExample1 = yesno (0 :: Int) -- False ... 0 is ambiguous
yesNoExample2 = yesno [] -- False
yesNoExample3 = yesno Nothing -- False
yesNoExample4 = yesno EmptyTree -- False
yesNoExample5 = yesno Green -- True

-- ** Now lets make a yesno if function with the YesNo typeclass
yesnoIf :: (YesNo yn) => yn -> a -> a -> a
yesnoIf cond yesVal noVal= if yesno cond then yesVal else noVal

yesnoIfExample1 = yesnoIf [] "Not Empty" "Empty"
yesnoIfExample2 = yesnoIf (Just 1) "Not Empty" "Empty"


-- ******* Functor typeclasses - have properties that can be mapped over (like - List, Maybe, Tree)
-- Types that can act like a box can be functors. You can think of a list as a box. Also, the Maybe a type in a way,
-- it's like a box that can either hold nothing or Just something. A tree can also be a functor
{- instance Functor [] where  
    fmap = map 
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing -}
-- fmap for tree isn't previously defined
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)   

myFmappedList = fmap (*2) [1..3]  

myFmappedMaybe1 = fmap (+2) Nothing -- Nothing
myFmappedMaybe2 = fmap (+2) $ Just 3 -- Just 5

myFmappedTree1 = fmap (*2) EmptyTree -- EmptyTree
myFmappedTree2 = fmap (*2) $ foldr insertToTree EmptyTree [8,6,4,1,7,3,5] -- Node 10 (Node 6 (Node 2 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree)) (Node 14 (Node 12 EmptyTree EmptyTree) (Node 16 EmptyTree EmptyTree))

-- ** Can Either a b be made a functor? The Functor typeclass wants a type constructor that takes only one type paramete
-- but Either takes two, so we can partially apply Either by feeding it only one parameter so that it has one free parameter
instance Functor (Either' a) where  
    fmap f (Right' x) = Right' (f x)  
    fmap f (Left' x) = Left' x  
-- The type signature would then be (b -> c) -> Either a b -> Either a c


-- ** NOTE: Maps from Data.Map can also be made a functor because they hold values (or not!). In the case of Map k v,
-- fmap will map a function v -> v' over a map of type Map k v and return a map of type Map k v'.
