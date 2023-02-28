import qualified Data.Map as Map
import Data.IntMap (fromList)
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) =if key == k then Just v else findKey key xs

-- Using fold

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

singleton k v = Map.insert k v Map.empty

phoneBookToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

phoneBookToMap' :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap' = Map.fromListWith (\number1 number2-> number1 ++ ","++number2)