module Calculator where

import ExprT ( ExprT(..) )
import Parser ( parseExp )


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


class Expr a where
    lit :: (Expr a) => Integer -> a
    mul :: (Expr a) => a -> a -> a
    add :: (Expr a) => a -> a -> a


instance Expr Integer where
    lit :: Integer -> Integer
    lit = id
    
    add :: Integer -> Integer -> Integer
    add a b = a + b
    
    mul :: Integer -> Integer -> Integer
    mul a b = a * b


instance Expr Bool where
    lit :: Integer -> Bool
    lit x
        | x < 0 = False
        | otherwise = True
    
    add :: Bool -> Bool -> Bool
    add a b = a || b

    mul :: Bool -> Bool -> Bool
    mul a b = a && b


instance Expr MinMax where
    lit :: Integer -> MinMax
    lit = MinMax

    add :: MinMax -> MinMax -> MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)

    mul :: MinMax -> MinMax -> MinMax
    mul (MinMax a) (MinMax b) = MinMax (min a b)


instance Expr Mod7 where
    lit :: Integer -> Mod7
    lit a = Mod7 (a `mod` 7)
    
    add :: Mod7 -> Mod7 -> Mod7
    add (Mod7 a) (Mod7 b) = Mod7 (a + b `mod` 7)
    
    
    mul :: Mod7 -> Mod7 -> Mod7
    mul (Mod7 a) (Mod7 b) = Mod7 (a * b `mod` 7)



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