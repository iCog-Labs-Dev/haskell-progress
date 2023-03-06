data Value = String | Int | Bool | Object | Array | Null

type Key = String

data ObjectData = ObjectData Key Value

type Object = [ObjectData]

type Number = Int

type Boolean = Bool

type Array = [Value]

type JSON = Object

-- sample = "{\"profile\":{\"id\":1234,\"name\" : \"kenenisa alemayehu\" }, \"fiends\" :\n[{\"id\":3456,\"name\":\"John the official 123\"},{\"id\":9846,\"name\":\"ed true\"}]}"
sample = "{content {is} this}wtf broo"

normalize :: [Char] -> Bool -> [Char]
normalize [] _ = []
normalize (x : xs) outSide
  | x == ' ' && outSide = normalize xs outSide
  | x == '"' && outSide = x : normalize xs False
  | x == '"' && not outSide = x : normalize xs True
  | x == '\n' || x == '\t' = normalize (tail xs) outSide
  | otherwise = x : normalize xs outSide

-- parseKeyValue :: String -> Object
-- parseKeyValue _ = []
-- parseKeyValue (x:xs) = 


parseObject :: String -> String -> String -> String
parseObject [] stack _ = error "ERROR: unable to parse object"
parseObject (x : xs) stack content
  | x == '}' && length stack == 1 = tail $ reverse content
  | x == '}' = parseObject xs (tail stack) (x : content)
  | x == '{' = parseObject xs (x:stack) (x:content)
  | otherwise = parseObject xs stack (x:content)


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
  putStrLn $ parseObject sample [] []
