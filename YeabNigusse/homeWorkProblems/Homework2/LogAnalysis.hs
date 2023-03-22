import Data.Char(isAlpha, isDigit, isSpace)
import Log
-- Excersise 1
parseMessage :: String -> LogMessage
parseMessage str
             | isValidString str = LogMessage (returnMessageType str) (returnTimeStamp str) (returnString str)
             | otherwise =  Unknown str

isValidString :: String -> Bool
isValidString str =  start == 'I' || start == 'E' || start == 'W'
                    where start = head str

returnMessageType :: String -> MessageType
returnMessageType str
                  | head str == 'I' = Info
                  | head str == 'W' = Warning
                  | head str == 'E' = Error (read y)
                  where (x:y:_) = words str
returnTimeStamp :: String -> TimeStamp
returnTimeStamp str
                | head str == 'E' = read z
                | otherwise = read y
                where (x:y:z: _) = words str
returnString :: String -> String
returnString str =
              let 
                (x:y:z:rest1) = words str
                (r:p:rest2) = words str
                 in if head str == 'E' then unwords rest1 else unwords rest2

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

--Excersis 2

singleton :: LogMessage -> MessageTree
singleton x = Node Leaf x Leaf

insert :: (Ord LogMessage) => LogMessage -> MessageTree -> MessageTree
insert x Leaf = singleton x
insert (LogMessage tp' time' str') (Node left (LogMessage tp time str) right)
             | tp' == tp = Node left (LogMessage tp' time' str') right
             | tp' < tp  = Node (insert (LogMessage tp' time' str') left) (LogMessage tp time str) right
             | tp' > tp = Node left (LogMessage tp time str) (insert (LogMessage tp' time' str') right)


