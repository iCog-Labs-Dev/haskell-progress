module LogAnalysis where

import Log
import Data.Char

parseMessage :: String -> LogMessage
parseMessage log
    | validLogType brokenLog =
            let stamp = read first
                message = concat (second:restOfList)
            in LogMessage (messageType $ head err) stamp message
    | validError brokenLog = 
            let errorValue = read first
                stamp = read second
                message = concat restOfList
            in LogMessage (Error errorValue) stamp message
    | otherwise = Unknown $ unwords brokenLog
    where brokenLog@(err:first:second:restOfList) = words  log

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

insert :: LogMessage -> MessageTree -> MessageTree
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ stampI _) (Node left currLog@(LogMessage _ stampT _) right)
    | stampI < stampT = Node (insert log left) currLog right
    | otherwise = Node left currLog (insert log right)
insert (Unknown _) messageTree = messageTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = filter (/="") logMessages
    where logMessages = map validLog sorted
          validLog (LogMessage (Error value) _ message) = if value >= 50 then message else ""
          validLog log = ""
          sorted = inOrder (build logs)

messageType :: Char-> MessageType
messageType 'I' = Info
messageType 'W' = Warning

validErrorType :: Char -> Bool
validErrorType x = x `elem` "IWE"

validLogType :: [String] -> Bool
validLogType (_: stamp : next : _) = all isDigit stamp && (not . all isDigit) next

validError :: [String] -> Bool
validError (_: errorValue : stamp : _) = all isDigit errorValue && all isDigit stamp

validLog :: [String] -> Bool
validLog log = validErrorType (errorType log) && (validLogType log || validError log)
    where errorType = head . head