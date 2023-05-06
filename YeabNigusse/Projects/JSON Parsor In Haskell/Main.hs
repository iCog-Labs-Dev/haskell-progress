module Main where
import Data.Char
import Control.Applicative 
data JsonValue =  JsonNull            {--this are values a json value has--}
                 | JsonBool Bool
                 | JsonNumber Integer
                 | JsonString String
                 | JsonArray [JsonValue]
                 | JsonObject [(String, JsonValue)]
                 deriving (Show, Eq)


newtype Parser a = Parser
 { runParser ::  String -> Maybe (String , a)}-- i don't understand it


instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do--to be applicative Parser should be a Functor type class
        (input', x) <- p input               -- making Parser an instance of functor type class
        Just (input' , f x)

instance Applicative Parser where
    pure :: a -> Parser a
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = -- to chain the result of each Parser type the parser should implement 
        Parser $ \input -> do     -- Applicatives
         (input', f) <- p1 input
         (input'', a) <- p2 input'
         Just (input'', f a)
         
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing  -- this is for JsonBool Parser type to use the true and false interchangebly 
    (Parser p1) <|> (Parser p2) =
         Parser $ \input ->  p1 input <|> p2 input 

jsonNull  :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null" -- JsonNull value constructor

charP :: Char -> Parser Char
charP x = Parser f
        where 
            f (y:ys)                    -- Parse characters from a given string 
              | y == x = Just (ys, x)
              | otherwise = Nothing
            f [] = Nothing
          
stringP  :: String -> Parser String
stringP = sequenceA  . map charP  -- implement string Parser by using Character parsing with advantage of
                                  -- Functors and applicatives

jsonBool :: Parser JsonValue
jsonBool =f <$> (stringP "true" <|> stringP "false")
          where f "true" = JsonBool True            -- used to Bool value by implementing Alternatives
                f "false" = JsonBool True
                f _ = undefined


spanP :: (Char -> Bool) -> Parser String
spanP f = 
    Parser $ \input -> 
        let (token, rest) = span f input  -- used to separest strings from numbers in jsonNumber value constructors
        in  Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser  $ \input -> do
        (input', xs)  <- p input   -- used to handel null case in jsonNumber value constructor
        if null xs
            then Nothing
            else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber =  f <$> notNull (spanP isDigit) -- JsoNumber value constructer
            where f ds =  JsonNumber $ read ds 

ws :: Parser String
ws = spanP isSpace
sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray =JsonArray <$> (charP '[' *> ws *>  -- a little bit complicated to explain but in general
                                    elements  -- used to Parse array of strings into JsonValue
                                    <* ws <* charP ']')

            where elements  = sepBy (ws *> charP ',' <* ws) jsonValue
                  sep = ws *> charP ',' <* ws          

stringLitral :: Parser String
stringLitral = (charP '"' *> spanP (/= '"')  <* charP '"')
    

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLitral

jsonObject :: Parser JsonValue
jsonObject =JsonObject  <$> (charP '{' 
             *> ws *> 
             sepBy (ws *> charP ',' <* ws) pair 
             <* ws <* 
             charP '}')
             where pair = 
                    (\ key _ value -> (key, value)) <$> stringLitral 
                                  <*> (ws *>  charP ':' <* ws) 
                                  <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonArray <|> jsonObject -- final associasion


parserFile :: FilePath -> Parser a -> IO (Maybe a)
parserFile fileName parser= do   -- used to test the parser from the file
    input <- readFile fileName
    return (snd <$>runParser parser input)

    
main :: IO ()
main = undefined


-- FUNCTORS APPLICATION OVER A FUNCTION
-- INCREASE CODE REUSABILITY

maybeInc :: Maybe Int -> Maybe Int
maybeInc Nothing = Nothing
maybeInc (Just x) = Just (x + 1)

listInc :: [Int] -> [Int]
listInc [] = []
listInc (x:xs) = (x+1) : listInc xs

data Tree a = Leaf a | Node a (Tree a) (Tree a)

treeInc :: (Num a) => Tree a -> Tree a
treeInc (Leaf a) = Leaf (a+1)
treeInc (Node a l r) = Node (a+1) (treeInc l) (treeInc r)

-- since we can define the above data types as an instance of Functor typeclass we can use inc function for each of them
inc :: Functor f => f Int -> f Int
inc = fmap (+1)


-- MONADS
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of
        Nothing -> Nothing
        Just n -> case eval' y of
              Nothing -> Nothing
              Just m -> safediv n m

type Time = Int

data Yeab a = Fellowship a | Class a | Work a | Internship a | Other a | UC a


-- GOAL - to make Yeab's time an instance of functors, applicative and monad

instance Functor Yeab where
    fmap f (Fellowship a) = Fellowship (f a)
    fmap f (Class a) = Class (f a)
    fmap f (Work a) = Work (f a)
    fmap f (Internship a) = Internship (f a)
    fmap f (Other a) = Other (f a)

instance Applicative Yeab where
    pure = UC
    Class a <*> Class b = fmap a (Class b)

    



          