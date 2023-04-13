module SimpleJson
(
Jvalue(..) -- visible functions and data types of this module
, getString
, getInt
, getDouble
, getBool
, getObject
, getArray
, isNull
) where

data Jvalue = JString String -- we are making our own data type by using primitive data types which is Json value
              | JNumber Double
              | JBool Bool  
              | JObject [(String, Jvalue)]
              | JArray [Jvalue]
              | JNull 
              deriving (Eq, Ord, Show)
-- we can do the reverse by using pattern matching

getString :: Jvalue -> Maybe String -- extracting value from json value
getString s = case s of
    (JString s) -> Just s
    _ -> Nothing

getInt :: Integral a => Jvalue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: Jvalue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: Jvalue -> Maybe Bool
getBool (JBool s) = Just s
getBool _ = Nothing

getObject :: Jvalue -> Maybe [(String, Jvalue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: Jvalue -> Maybe [Jvalue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: Jvalue -> Bool
isNull v = v == JNull

