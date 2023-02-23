-- ************** Type and Type classes
-- "hello" :: [char], (True, 'a', 1) :: (Bool, Char, Num) ... :: means type of

-- removeNonUppercase :: [Char] -> [Char]  -- type declaration ... input parameter -> return value
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   -- function implelmentation   

addThree :: Int -> Int -> Int -> Int  -- param1 -> param2 -> param3 -> return value
addThree x y z = x + y + z  

circumference :: Float -> Float -- is a real floating point with single precision.
circumference r = 2 * pi * r  
circumference' :: Double -> Double  -- is a real floating point with double precision.
circumference' r = 2 * pi * r  

-- ******** Typeclass
-- :t (==)  will return ... (==) :: (Eq a) => a -> a -> Bool  ... everything before '=>' is called a 'Class Constraint'
-- This is read as:  the equality function takes any two values that are of the same type and
-- returns a Bool. The type of those two values must be a member of the "Eq typeclass". 
-- All haskell types (except IO) are part of Eq typeclass. `elem` has type (Eq a) => a -> [a] -> Bool because it uses '==' somewhere

-- ** :t(==), :t(/=) ... :: Eq a => a -> a -> Bool 
eqType = 5 /= 5 -- returns False
-- ** <, >, >=, <=, `compare` ... :: (Ord a) => a -> a -> Bool ... all types except for functions and IO are part of Ord
compareType = "Abrakadabra" `compare` "Zebra"  -- returns 'LT'
-- ** show ... :: Show a => a -> String
showString = show 5.334 -- "5.34", show True = "True" 
-- ** read ... :: Read a => String -> a
readString = read "5.34" + 2 -- 7.34, read "True" || False = True, read "4" = ERROR (ambiguous)
readString2 = read "5" :: Int -- 5, read "5.34" :: Float = 5.34, read "(3,'a')" :: (Float, Char) = (3.0, 'a')
-- ** succ, pred ... :: Enum a => a -> a ... (Enum types are sequentially ordered types)
successor = succ 'B' -- 'C'
successor2 = [LT .. GT] -- [LT,EQ,GT], [1..9], ['a'..'z']
-- ** minBound, maxBound ... :: Bounded a => a
minBoundOfInt = minBound :: Int -- -2147483648  
maxBoundOfThisTriple = maxBound :: (Bool, Int, Char)   -- (True,2147483647,'\1114111')
-- ** ...,-2,-1,0,1,2,... ... :: Num a => a
num1 = 20 :: Int -- 20, 20 :: Integer = 20
num2 = 20 :: Float -- 20.0, 20 :: (Double) = 20.0
-- ** ---, -0.2,-0.1,0.0,... ... :: (Fractional a) => a
num3 = 20.0
-- ** +, - ... :: Num / Integral a => a -> a -> a <<>> / :: Fractional a => a -> a -> a
-- ** fromIntegral :: (Integral a, Num b) => a -> b >> it has multiple class constraints in its type signature (Int, Float)
addIntAndFloat = fromIntegral (length [1,2,3,4]) + 3.2
-- 'fromIntegral' takes an integral number and turns it into a more general number. 
-- That's useful when you want integral and floating point types to work together.