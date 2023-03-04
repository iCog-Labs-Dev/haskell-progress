module LogAnalysis where

import Log (LogMessage (LogMessage, Unknown), MessageTree (Leaf, Node), MessageType (Error, Info, Warning), TimeStamp, testParse, testWhatWentWrong)

main :: IO ()
main = do
  logs <- testParse parse 30 "error.log"
  mapM_ print logs
  whatwentwrong <- testWhatWentWrong parse whatWentWrong "sample.log"
  mapM_ print whatwentwrong

parseMessage :: String -> LogMessage
parseMessage ('I' : _ : xs) =
  let msgType = Info
      splited = words xs
      timeStamp = read (head splited) :: Int
      msg = unwords $ tail splited
   in LogMessage msgType timeStamp msg
parseMessage ('W' : _ : xs) =
  let msgType = Warning
      splited = words xs
      timeStamp = read (head splited) :: Int
      msg = unwords $ tail splited
   in LogMessage msgType timeStamp msg
parseMessage ('E' : _ : xs) =
  let msgType = Error errorCode
      splited = words xs
      errorCode = read (head splited) :: Int
      timeStamp = read (splited !! 1) :: Int
      msg = unwords $ tail $ tail splited
   in LogMessage msgType timeStamp msg
parseMessage any = Unknown any

parse :: String -> [LogMessage]
parse = map parseMessage . lines

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timestamp _) = timestamp

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error severity) _ _) = severity

isErrorMessage :: LogMessage -> Bool
isErrorMessage (LogMessage (Error _) _ _) = True
isErrorMessage _ = False

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ timestamp _) (Node left msg right)
  | timestamp <= nodeTimeStamp = Node (insert logMsg left) msg right
  | otherwise = Node left msg (insert logMsg right)
  where
    nodeTimeStamp = getTimeStamp msg

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter (\x -> getSeverity x > 50) . filter isErrorMessage . inOrder . build