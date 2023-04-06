module Scratch where

data S = Plus D S | Digit D deriving Show
data D = Zero | One deriving Show

printS :: S -> String
printS (Digit d) = printD d
printS (Plus d s) = printD d ++ "+" ++ printS s

printD :: D -> String
printD Zero = "0"
printD One  = "1"

evalS :: S -> Int
evalS (Digit d) = evalD d
evalS (Plus d s) = evalD d + evalS s

evalD :: D -> Int
evalD Zero = 0
evalD One = 1

type Parser = String -> Maybe String

satisfies :: (Char -> Bool) -> Parser
satisfies _ [] = Nothing
satisfies p (c:cs)
    | p c = Just cs
    | otherwise = Nothing

char :: Char -> Parser
char c = satisfies (== c)

digit :: Parser
digit = satisfies (`elem` ['0'..'9'])

letter :: Parser
letter = satisfies (`elem` ['a'..'z'] ++ ['A'..'Z'])

eof :: Parser
eof [] = Just []
eof _ = Nothing

combine :: Parser -> Parser -> Parser
combine p1 p2 s = do
    s' <- p1 s
    p2 s'

many :: Parser -> Parser
many p s = case p s of
    Nothing -> Just s
    Just s' -> many p s'

parseTree :: S
parseTree = Plus
                One
                (Plus
                    Zero
                    (Digit One))

