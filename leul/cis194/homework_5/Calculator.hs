module Calculator where

import ExprT ( ExprT(..) )
import Parser ( parseExp )



eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2


evalStr :: String -> Maybe Integer
evalStr str = case parsed of
  Just a -> Just (eval a)
  Nothing -> Nothing
  where
    parsed = parseExp Lit Add Mul str