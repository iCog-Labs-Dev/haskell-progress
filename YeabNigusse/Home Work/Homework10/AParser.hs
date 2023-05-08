module AParser where

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

-- Excercise 1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =  Parser $ \input -> do-- this is a mission imposible for finding(penetrating and entering) a
             (x, input') <- p input
             Just (f x, input')

-- Excersise 2

instance Applicative Parser where
      pure :: a -> Parser a
      pure a = Parser $ \input -> Just (a, input)
      (<*>) :: Parser (a -> b) -> Parser a -> Parser b
      (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do
        (f, input') <- p1 input
        (a, input'') <- p2 input'
        Just (f a , input'')

-- Excersise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- *AParser> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- *AParser> runParser abParser "aebcdf"
-- Nothing


abParser_ :: Parser ()
abParser_ = mempty <$> char 'a' <*> char 'b'

-- *AParser> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- *AParser> runParser abParser_ "aebcdf"
-- Nothing


intPair :: Parser [Integer]
intPair =  (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

-- Excersise 4
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

--Exercise 5

intOrUppercase :: Parser ()
intOrUppercase =  undefined-- i want to find out what happened here


-- *Parser> runParser intOrUppercase "342abcd"
-- Just ((), "abcd")
-- *Parser> runParser intOrUppercase "XYZ"
-- Just ((), "YZ")
-- *Parser> runParser intOrUppercase "foo"
-- Nothing
        
-- ABOUT SIDE EFFECTS

-- FUNCTIONS THAT INTERACT WITH THE OUTSIDE WORLD(INPUT OUTPUT, NETWORK ACESS, SYSTEM ACCESS etc) usually
-- HAVE SIDE EFFECTS(THEY DO SOMETHING TO THE OUTSIDE WORLD RATHER THAN BEING INSIDE THE PROGRAMM)
-- THEY GIVE DIFFERENT OUTPUT FOR THE SAME INPUT OR CALL(e.g RANDOM, CURRENT_TIME, GLOBAL_FUNC -FUNCTIONS)


-- ABOUT FUNCTORS

-- SOMETHING TO BE A FUNCTOR INSTANCE IT SHOLD IMPLEMENT FMAP FUNCTION WHICH MAPS A FUNCTION INTO A FUNCTOR
-- OR FMAP IS A FUNCTION THAT RETURN A LIFTED FUNCTION WHICH LIFT UP A FUNCTOR
-- (e.g) IO, MAYBE, PARSER -> LET US WRITE THEM EVEN IF THEY ARE ALREDY IMPLEMENTED


