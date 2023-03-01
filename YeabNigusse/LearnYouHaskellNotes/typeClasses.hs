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

data Book = Book {
    title :: String,
    auter :: String,
    pages :: Int
}deriving (Show)
-- Book {title = "wsane", auter = "eyob mamo", pages = 120}
-- that is more descriptive and shortest way to write
