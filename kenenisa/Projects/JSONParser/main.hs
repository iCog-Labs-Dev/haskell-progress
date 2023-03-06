type NumberType = Int
data Value = ValueString String | Number Int | Boolean Bool | Object [(String,Value)] | Array [Value] | Null 
    deriving (Read,Show)


sample = "{\"profile\":{\"id\":1234,\"name\" : \"kenenisa alemayehu\" }, \"fiends\" :\n[{\"id\":3456,\"name\":\"John the official 123\"},{\"id\":9846,\"name\":\"ed true\"}]}"

digits :: String
digits = ['0' .. '9']

normalize :: [Char] -> Bool -> [Char]
normalize [] _ = []
normalize (x : xs) outSide
  | x == ' ' && outSide = normalize xs outSide
  | x == '"' && outSide = x : normalize xs False
  | x == '"' && not outSide = x : normalize xs True
  | x == '\n' || x == '\t' = normalize (tail xs) outSide
  | otherwise = x : normalize xs outSide

parseQuotes :: String -> Bool -> String -> (Value, String)
parseQuotes [] _ _ = error "ERROR: unable to parse string"
parseQuotes (x : xs) open acc
  | x == '"' && open = (ValueString $ reverse acc, xs)
  | x == '"' && not open = parseQuotes xs True acc
  | otherwise = parseQuotes xs open (x : acc)

parseNumber :: String -> String -> (Value, String)
parseNumber (x : xs) acc
  | x == ',' = (Number (read (reverse acc) :: Int), xs)
  | x `notElem` digits = error "ERROR: unable to parse number"
  | otherwise = parseNumber xs (x : acc)

extractKey :: Value -> String
extractKey (ValueString key) = key

parseKeyValue :: String -> [(String,Value)]
parseKeyValue [] = []
parseKeyValue (x : xs) 
  | x == '"' =  (extractKey key, fst value) : parseKeyValue (snd value)
  | otherwise = parseKeyValue xs
  where
    parsedKey = parseQuotes xs False []
    key = fst parsedKey
    parsedValue = tail $ tail $ snd parsedKey
    value :: (Value, String)
    value
      | head parsedValue == '{' =  (Object $ fst objectRule,snd objectRule)
      | head parsedValue == '"' = parseQuotes parsedValue False []
      | head parsedValue == 't' = (Boolean True, drop 4 parsedValue)
      | head parsedValue == 'f' = (Boolean False, drop 5 parsedValue)
      | head parsedValue == 'n' = (Null, drop 4 parsedValue)
      | head parsedValue `elem` digits = parseNumber parsedValue []
      | head parsedValue == '[' = error "Not implemented yet"
      | otherwise = error "Error: don't know what to parse value to"
        where objectRule = parseObject parsedValue [] []

parseObject :: String -> String -> String -> ([(String,Value)],String)
parseObject [] stack _ = error "ERROR: unable to parse object"
parseObject (x : xs) stack content
  | x == '}' && length stack == 1 =  (parseKeyValue $ tail $ reverse content, xs)
  | x == '}' = parseObject xs (tail stack) (x : content)
  | x == '{' = parseObject xs (x : stack) (x : content)
  | otherwise = parseObject xs stack (x : content)

fromJSON :: a
fromJSON = error "hey"

printer :: String -> IO ()
printer [] = return ()
printer (x : xs) = do
  putStrLn [x]
  printer xs

main :: IO ()
main = do
  printer sample
  print $ Object $ fst $ parseObject sample [] []
