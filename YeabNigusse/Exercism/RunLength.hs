module RunLength (decode, encode) where
import Data.List ( group )
import Data.Char (isDigit, isAlpha)
decode :: String -> String
decode txt = addSpace (func (removeSpace txt))

encode :: String -> String
encode txt
           | txt == "" = ""
           | otherwise = removezero (concat' (tozero (returnlength (groupword txt)))  (takeone (groupword txt)))

groupword :: String -> [String]
groupword  = group

returnlength :: [String] -> [Int]
returnlength [] = []
returnlength (x:xs) = length x : returnlength xs

takeone :: [String] -> String
takeone [] = []
takeone (x:xs) = take 1 x ++ takeone xs

toString :: [Int] -> String
toString [] = ""
toString (x:xs) = show x ++ toString xs

tozero :: [Int] -> [Int]
tozero [] = []
tozero (x:xs) = if x == 1 then 0 : tozero xs else x : tozero xs

concat' :: [Int] -> String -> String
concat' [] _ = []
concat' _ [] = []
concat' (x:xs) (y:ys) = show x ++ y : concat' xs ys

removezero :: String -> String
removezero [] = ""
removezero (x:xs) = if x == '0' then removezero xs else x:removezero xs


func :: String -> String
func [] = ""
func str 
           | null str1 = head str: func (tail str)
           | otherwise = let [(myint, mystr)] = str1
                             (y:reststr) = mystr
                      in replicate myint y ++ func reststr
                    where str1 = reads str :: [(Int, String)]
removeSpace :: String -> String
removeSpace [] = ""
removeSpace (x:xs) = if x == ' ' then '_': removeSpace xs else x:removeSpace xs

addSpace :: [Char] -> [Char]
addSpace [] = ""
addSpace (x:xs) = if x == '_' then ' ': addSpace xs else x:addSpace xs