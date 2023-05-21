import Data.List(intersperse,nub,concat, concatMap, break, inits, tails)
import Data.Char(toLower, ord, chr)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- [].nub returns a list that contain uniqe elements only

uniqList :: (Eq a) => [a] -> Int
uniqList = length . nub

uniqString :: String -> Int
uniqString = length . nub

--Let US WRITE A FUNCTION THAT CHECK IF A STRING CONTAIN EQUAL UNIQ LENGTH 

compareAlphabet :: String -> String -> Bool
compareAlphabet text1 text2 = uniqString (map toLower text1) == uniqString (map toLower text2)

-- let us practice SOME  Data.List Functions

insert :: [Integer]
insert = intersperse 0 [1,2,3,4,5]

concatList :: [String] -> String
concatList  = concat   -- concatList ["Yeab", "Sami", "Yoni"]


concatList' :: [Integer]
concatList' = concatMap  (replicate 2) [1,2]

-- CHECK IF A CHARACTER IS FOUND IN A STRING USING ANY
isFound :: Char ->String -> Bool
isFound x text = any (== x) text

-- DOING THE ABOVE PROBLEM BY USING OR

isFound' :: Char -> String -> Bool
isFound' x text = or $ map (== x) text

-- LET US SPLIT OUR SENTENCE INTO TUPLES

breakList :: [Char] -> ([Char], [Char])
breakList  = span (==' ') -- change span with break and see the diffrence 

-- LET US BREAK OUR WORD
breakWord word = inits word 
breakWord' word = tails word 

keepWord word = map (uncurry (++)) $ (zip (inits word)  (tails word ))
-- LET US ADD THREE LISTS TOGETHER INTO ONE
addlist = zipWith3 (\x y z -> x+y+z)  [1,2 ..5] [1,2 ..5] [1,2 ..5]

-- LET US ENCODE A STRING 

encode :: String -> Int -> String
encode msg shift = let 
                   ords = map ord msg
                   shifted = map (+shift) ords
                   in map chr shifted
-- LET US DECODE THE ABOVE ENCODED STRING

decode :: String -> Int -> String
decode msg  shif = encode msg (negate shif)

-- DATA.MAP
-- LET US FIND A VALUE BY USING A KEY

phoneBook :: [(String, String)]
phoneBook = [("betty","555-2938") ,
            ("bonnie","452-2928") ,
            ("patsy","493-2928"),
            ("lucille","205-2928"),
            ("wendy","939-8282"),
            ("penny","853-2492")]  

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd .head . filter (\(k,v) -> k ==key) $ xs 

-- let us refine it to avoid errors

findKey' :: (Eq k ) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) = if key == k then Just v else findKey' key xs
-- test the above code by using (findKey' "betty" phoneBook) and (findKey' "yeab" phoneBook )
 
-- LET US MAP AND FILTER A MAP
myMap :: (Ord k) => [(k,v)] -> Map.Map k v
myMap = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty -- how can i insert my key value pair into it?

squareNumbers :: [(Int, Int)]
squareNumbers = [(1,1), (2,4), (3,9), (5,25)] -- i want to do some mapping and filtering into it but i don't understand how it works


-- DATA.SET MODULE

--we use the to remove any replicated value found in a list
-- Set.formList - USED TO CHANGE INTO SET
-- Set.intersection set1 set2



