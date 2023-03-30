module LogAnalysis where

import Data.List (intercalate)
import Data.Typeable (typeOf)
import Log

createLogMessage :: String -> MessageType -> LogMessage
createLogMessage xs msgType =
  let splitedXs = words xs
   in LogMessage msgType (read $ head splitedXs :: Int) (unwords $ tail splitedXs)

parseMessage :: String -> LogMessage
parseMessage ('I' : xs) = createLogMessage xs Info
parseMessage ('W' : xs) = createLogMessage xs Warning
parseMessage ('E' : xs) =
  let splitedXs = words xs
   in createLogMessage (unwords $ tail splitedXs) (Error (read $ head splitedXs :: Int))
parseMessage (_ : xs) = Unknown xs

parse :: String -> [LogMessage]
parse xs = foldl (\acc x -> parseMessage x : acc) [] (lines xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert ilogMsg Leaf = Node Leaf ilogMsg Leaf
insert ilogMsg@(LogMessage _ its _) (Node left logMsg@(LogMessage _ ts _) right) =
  if its > ts
    then Node left logMsg (insert ilogMsg right)
    else Node (insert ilogMsg left) logMsg right

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsg = foldl filterError [] (inOrder (build logMsg))
  where
    errorSeverity e = e >= 50 || False
    filterError :: [String] -> LogMessage -> [String]
    filterError xs logMsg@(LogMessage (Error e) _ str) = if errorSeverity e then xs ++ [str] else xs
    filterError xs _ = xs