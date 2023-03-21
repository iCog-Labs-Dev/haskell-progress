module Main where
import Data.Char
import Control.Applicative
data JsonValue = JsonNull 
                | JsonBool Bool 
                | JsonNumber Integer 
                | JasonString String 
                | JsonArray [JsonValue] 
                | JsonObject [(String, JsonValue)] deriving (Show, Eq)-- this are possible values that 
                                                                      -- a string is being parsed
            
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}{-- a string my be parsed to a some json value with 
                                                                        other string that can be chained or (no proper error reporting)   --}

                                                                        

charP :: Char -> Parser Char
charP x = Parser f 
          where f (y:ys)
                    | y == x = Just(ys, y)
                    | otherwise = Nothing
                f [] = Nothing

instance Functor Parser where 
    fmap f (Parser p) = 
        Parser $ \input -> do
            (input', x) <- p input
            Just (input, f x)

instance Applicative Parser where 
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do
            (input', f) <- p1 input 
            (input'', a) <- p2 input' 
            Just (input'', f a)

instance Alternative Parser where 
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"


jsonBool :: Parser JsonValue
jsonBool = f <$>(stringP "true" <|> stringP "false")
           where f "true" = JsonBool True
                 f "false" = JsonBool False 
                 f _ = undefined
spanP :: (Char -> Bool) -> Parser String
spanP f =
     Parser $ \input -> 
                 let (token, rest) = span f input 
                     in Just (rest, token)


jsonNumber :: Parser JsonValue
jsonNumber =  f <$> spanP isDigit
    where f ds  = JsonNumber $ read ds

jsonValue :: Parser JsonValue 
jsonValue = undefined


main :: IO ()
main =  undefined

