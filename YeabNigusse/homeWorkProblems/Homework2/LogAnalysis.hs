import Data.Char(isAlpha, isDigit, isSpace)
import Log ( LogMessage(..), MessageType(Warning, Info, Error) ) 

parseMessage :: String -> LogMessage
parseMessage str
              | head str == 'I' = LogMessage Info (read (filter isDigit str)) (filter (not . isDigit) (tail str))
              | head str == 'E' = LogMessage (Error  (read (head (tail (filter (not . isSpace) str))))) 4 "dd"
              | head str == 'W' = LogMessage Warning 2 "g"
              | otherwise = Unknown str
