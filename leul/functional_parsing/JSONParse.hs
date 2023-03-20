module JSONParse where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Parsing
    ( Alternative((<|>), many), Parser, parse, sat, char, string, int )
import Data.Char ( isSpace )


-- Definitions
-- ----------------------------------- --
-- object   - {key:value} | {*key:value,}
-- key      - string
-- value    - object | string | number | boolean | array | null
-- string   - "alphanumeric"
-- boolean  - true | false
-- array    - [*value,] | [value] | []


data Value = Null | JString String | Number Double | Boolean Bool | Object [(String, Value)] | Array [Value]
    deriving Show


cleanSpace :: Bool -> String -> String
cleanSpace _ [] = []
cleanSpace inQuote (x:xs)
  | x == '"'                 = x : cleanSpace (not inQuote) xs
  | not inQuote && isSpace x = cleanSpace inQuote xs
  | otherwise                = x : cleanSpace inQuote xs
  


nonquote :: Parser Char
nonquote = sat (`notElem` ['"'])


num :: Parser Value
num = do x <- int
         char '.'
         y <- int
         return $ Number (read (show x ++ "." ++ show y)  :: Double) 
        <|>         
         Number . fromIntegral <$> int


jnull :: Parser Value
jnull = do string "null"
           return Null
           

str :: Parser Value
str = do char '"'
         x <- many nonquote
         char '"'
         return (JString x)


bool :: Parser Value
bool = do x <- string "true" <|> string "false"
          case x of 
            "true" -> return (Boolean True)
            "false" -> return (Boolean False)


value :: Parser Value
value = do jnull <|> bool <|> num <|> str <|> object <|> array
           

object :: Parser Value
object = do char '{'
            kvs <- many keyValue                          
            char '}' 
            return (Object kvs)


keyValue :: Parser (String, Value)
keyValue = do k <- str              
              char ':'
              v <- value
              char ','
              return ((\(JString a) -> a) k, v)
            <|>
              do k <- str
                 char ':'
                 v <- value                 
                 return ((\(JString a) -> a) k, v)


array :: Parser Value
array = do char '['
           vs <- many element
           char ']'
           return (Array vs)


element :: Parser Value
element = do vs <- value
             char ','
             return vs
         <|> value




main :: IO ()
main = do putStrLn "Enter json filepath: "
          
          filename <- getLine
          
          withFile filename ReadMode (
                        \handle -> 
                            do json <- cleanSpace False <$> hGetContents handle                    
                               
                               let parsed = parse object json
                               
                               if length parsed == 1 
                                then print $ fst $ head parsed
                                else putStrLn "Error while parsing json!"                       
                        )