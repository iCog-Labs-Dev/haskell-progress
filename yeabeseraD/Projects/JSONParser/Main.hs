module Main where

import Data.Char
import Control.Applicative

data JsonValue = JsonNull
    | JsonBool Bool
    | JsonNumber Integer
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)] 
    deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where 
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (input, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', x) <- p2 input'
        Just (input'', f x)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser (const Nothing)
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p) <|> (Parser p') = Parser $ \input -> p input <|> p' input

jsonValue :: Parser JsonValue
jsonValue = jsonBool <|> jsonNull <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true" = JsonBool True
          f "false" = JsonBool False
          f _ = undefined

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
        (input', xs) <- p input
        if null xs 
            then Nothing
            else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f ds = JsonNumber $ read ds

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
    where pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
                             in Just (rest, token)

charP :: Char -> Parser Char
charP x = Parser f
    where f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
          f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)

main :: IO()
main = undefined
