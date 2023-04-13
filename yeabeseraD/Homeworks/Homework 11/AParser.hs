{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
      | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

---newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do 
    result <- p input
    Just $ first f result

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just(x , input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input') <- p1 input
    (x, input'') <- p2 input'
    Just (f x, input'')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = mempty <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair =  (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = Parser (\input -> do
  (_, rest) <- runParser posInt input
  Just ((), rest)
  )
  <|> 
  Parser (\input -> do
    let parseChar = satisfy isUpper
    (_, rest) <- runParser parseChar input
    Just ((), rest)
  )