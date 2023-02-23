
data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Square Point Float deriving (Show)

data Person = Person {
    firstName :: String,
    lastName  :: String             
} deriving (Show)

data Vector a = Vector2 a a | Vector3 a a a deriving (Show, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector2 a b) `vplus` (Vector2 e f) = Vector2 (a+e) (b+f)
(Vector3 a b c) `vplus` (Vector3 e f g) = Vector3 (a+e) (b+f) (c+g)

areEqual = Vector2 1 1 == Vector2 1 1
areNotEqual = Vector3 1 2 3 /= Vector2 1 2