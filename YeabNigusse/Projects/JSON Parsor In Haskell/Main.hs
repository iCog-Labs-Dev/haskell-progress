module Main where

data JsonValue = JsonNull 
                | JsonBool Bool 
                | JsonNumber Integer 
                | JasonString String 
                | JsonArray [JsonValue] 
                | JsonObject [(String, JsonValue)] deriving (Show, Eq)-- this are possible values that 
                                                                      -- a string is being parsed
            
newtype Parsor a = Parsor {runParser :: String -> Maybe (String, a)}{-- a string my be parsed to a some json value with 
                                                                        other string that can be chained or (no proper error reporting)   --}

jsonValue :: Parsor JsonValue -- 
jsonValue = undefined


main :: IO ()
main =  undefined

