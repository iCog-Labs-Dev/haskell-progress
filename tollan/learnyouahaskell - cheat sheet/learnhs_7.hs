import Data.List
-- import Data.List (nub, sort) -- import some specific functions
-- import Data.List hiding (nub)   -- import all functions except the selected ones
import qualified Data.Map  --  if we want to reference Data.Map's filter function, we have to do Data.Map.filter
    -- whereas just filter still refers to the normal filter from 'Prelude' module
import qualified Data.Map as Map  --  to reference Data.Map's filter function, we just use M.filter
import Data.Function (on)
import Data.Char (ord, chr)
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub -- \xs -> length (nub xs) .. 'nub' is a function that takes in a list and weeds out duplicates
nubExample = nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  -- [1,2,3,4]  

groupExample = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] -- [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

atOperator = \xs@(x:rest) -> if x==0 then rest else xs
atOperatorExample = atOperator [0,0,00,1,2,3,0,5,6,7]

-- ** Searching for a sublist ... needle in a haystack
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (Data.List.tails haystack)  
-- ** Or simply
searchExample = "cat" `isInfixOf` "im a cat burglar" -- isPrefixOf isSuffixOf

-- ** Find an element that matches the condition
findExample1 = find (>4) [1,2,3,4,5] -- Just 5 ... Just :: a -> Maybe
findExample2 = find (>4) [8,7,6,5,4] -- Just 8
findExample3 = find (>4) [0,1,2,3,4] -- Nothing ... Nothing :: Maybe a

-- ** Find the index of the first element that is specified
elemIdxExample1 = 4 `elemIndex` [1,2,3,4,5,6] -- Just 3
elemIdxExample2 = 'a' `elemIndex` "Hello" -- Nothing
-- ** Find the indices of all the elements that are specified
elemIdxsExample1 = 3 `elemIndices` [3,4,3,3,5,3] -- [0,2,3,5]
elemIdxsExample2 = 'b' `elemIndices` "Still Hello" -- []

-- ** List of lines in a string
groupLines = lines "first line\nsecond line\nthird line" -- ["first line","second line","third line"]  
unlineGroups = unlines ["first line", "second line", "third line"] -- "first line\nsecond line\nthird line\n"
groupWords = words "hey these           are    the words in this\nsentence"  -- ["hey","these","are","the","words","in","this","sentence"]
unWordGroups = unwords ["hey","there","mate"]  -- "hey there mate" 

-- ** Delete the first occurence of an element from a List
deleteExample =  delete 'h' "hey, hi, hello" -- "ey, hi, hello" 
-- ** Delete the first occurence of a list from a list
withoutExample = "Im a big, big baby" \\ "big" -- "Im a , big baby"

-- Note that:
-- What length, take, drop, splitAt, !! and replicate all take an Int as one of their parameters (or return an Int)
-- Data.List has their more generic equivalents, named genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate

-- The nub, delete, union, intersect and group functions all have their more general counterparts called
-- nubBy, deleteBy, unionBy, intersectBy and groupBy. first set of functions use == whereas the second ones take an equality function
groupValues = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
groupbyExample1 = groupBy (\x y -> (x > 0) == (y > 0)) groupValues -- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]] 
groupbyExample2 = groupBy ((==) `on` (> 0)) groupValues -- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]  
--  doing (==) `on` (> 0) returns an equality function that looks like \x y -> (x > 0) == (y > 0)
--  sortBy, insertBy, maximumBy and minimumBy take a function that determine if one element is greater, smaller or equal to the other.
-- 'on' is used a lot on by functions. E.g., compare `on` length ..... \x y -> length x `compare` length y
onSortExample = sortBy (compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]] -- [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

-- ******** Data.Char
-- The ord and chr functions convert characters to their corresponding numbers and vice versa
ordExample1 = ord 'a' -- 97 ... ord 97 = 'a'
ordExample2 = map ord "abcdefgh" -- [97,98,99,100,101,102,103,104]
ceaserCipher :: Int -> String -> String
ceaserCipher key msg = map chr $ map (+key) $ map ord msg

-- ******** Data.Map
-- Association Lists / Maps / Dictionaries are used to store key value pairs where ordering doesn't matter (unlike sets)
-- Data.Map exports functions that clash with the Prelude and Data.List ones, we'll do a qualified import.
-- import qualified Data.Map as Map  

-- ** The fromList function takes an association list (in the form of a list) and returns a map with the same associations.
-- Type signature ... Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v 
mapExample1 = Map.fromList [(1,2),(3,4),(3,2),(5,5)] -- fromList [(1,2),(3,2),(5,5)]
mapExample3 = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 
mapExample2 = Map.fromList [(1,"One"),(2,"Two"),(3,"Three"),(3,"Number Three")] -- The second '3' is discarded 
-- If there are duplicate keys in the original association list, the duplicates are just discarded. This is the type signature of


-- ************ Data.Set
--  All the elements in a set are unique and ordered.
-- Because the names in Data.Set clash with a lot of Prelude and Data.List names, we do a qualified import.
-- import qualified Data.Set as Set

-- The fromList function takes a list and converts it into a set.
listToSet = "The old man left his garbage can out and now his trash is all over my lawn!"
setExample1 = Set.fromList listToSet -- fromList " !Tabcdefghilmnorstuvwy"