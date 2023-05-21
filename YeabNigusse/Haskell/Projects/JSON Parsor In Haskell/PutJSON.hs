module PutJSON where

-- let us convert haskell json value to appropriate json string by rendering our json value

import Data.List(intercalate)
import SimpleJson

renderJValue :: Jvalue -> String
renderJValue (JNumber number) = show number -- {"age":30}
renderJValue (JString string) = string -- {"name":"John"}
renderJValue (JBool True) = "true" --{"sale":true} 
renderJValue (JBool False) = "false"
renderJValue JNull = "null" --{"middlename":null} 
renderJValue (JObject o) = "{ " ++ pairs o ++ " }"
                  where pairs [] = ""
                        pairs obj = intercalate ", " (map renderpair obj)
                        renderpair (k, v) = show k ++ ": " ++ renderJValue v  --"employee":{"name":"John", "age":30, "city":"New York"}

renderJValue (JArray a) = "[" ++ values a ++ "]"
                    where values [] = ""
                          values vs = intercalate ", " (map renderJValue vs) --"employees":["John", "Anna", "Peter"]

putJValue :: Jvalue -> IO ()
putJValue v = putStrLn (renderJValue v)

{--
ghci> putJValue (JString "Hello")
Hello

ghci> putJValue (JArray [(JNumber 3),(JNumber 3),(JNumber 3),(JNumber 3)])
[3.0, 3.0, 3.0, 3.0]

ghci> putJValue (JObject [("name", (JString "Yeabsira")), ("age", (JNumber 21))])
{ "name": Yeabsira, "age": 21.0 }

so i think we are able to do convert haskell json value to approprate json data MAINLY TO STRING FORMAT
steps
-- Declar data type of json value called Jvalue
-- extract each type of jvalue to its approprate json data by using renderJValue function
--}


