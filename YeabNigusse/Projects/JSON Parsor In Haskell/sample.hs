import Data.Char (isDigit)

type Parser a = String -> Maybe (String , a)

integer :: Parser Int
integer input = case span isDigit input of 
                ("", _) ->  Nothing
                (digit, rest) -> Just (rest, read digit)