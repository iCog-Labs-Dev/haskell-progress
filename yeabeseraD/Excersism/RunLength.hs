module RunLength (decode, encode) where
import Data.List (group) 

decode :: String -> String
decode [] = []
decode text@(x:xs)
    | null countStr = x: decode xs
    | otherwise = let [(count, str)] = countStr; (y:ys) = convertToSpaces str
                  in replicate count y ++ decode ys
    where countStr = reads (convertSpaces text) :: [(Int, String)]

convertSpaces :: String -> String
convertSpaces =  map (\x -> if x == ' ' then '_' else x)

convertToSpaces :: String -> String
convertToSpaces = map (\x -> if x == '_' then ' ' else x)
encode :: String -> String
encode text = concatMap countAndAttach (group text)
    where countAndAttach x = if length x > 1 
                                then show (length x) ++ [head x] 
                                else [head x]
