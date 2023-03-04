module LogAnalysis where
import Log ( LogMessage(LogMessage, Unknown), MessageType(Error, Info, Warning), testParse )

 
parseMessage :: String -> LogMessage
parseMessage ('I':_:xs) = let msgType = Info 
                              splited = words xs
                              timeStamp = read (head splited) :: Int
                              msg = unwords $ tail splited
                          in LogMessage msgType timeStamp msg

parseMessage ('W':_:xs) = let msgType = Warning 
                              splited = words xs
                              timeStamp = read (head splited) :: Int
                              msg = unwords $ tail splited
                          in LogMessage msgType timeStamp msg

parseMessage ('E':_:xs) = let msgType = Error errorCode
                              splited = words xs
                              errorCode = read (head splited) :: Int
                              timeStamp =  read (splited !! 1) :: Int         
                              msg = unwords $ tail $ tail splited                          
                          in LogMessage msgType timeStamp msg

parseMessage any = Unknown any



parse :: String -> [LogMessage]
parse = map parseMessage . lines


main :: IO ()
main = do 
    logs <- testParse parse 30 "error.log"
    mapM_ print logs