import ExprT ( ExprT(..) )
import Parser
-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Just x -> Just (eval x)
    Nothing -> Nothing

-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a
-- NOTE- ALWAYS START WITH CLASS NAME AND SOME VARIABLE(LATTER REPLACED BY DATA TYPE)
-- ALL CALSS SHOULD HAVE SOME METHOD(FUNCTION) FOR WHICH OTHER DATA TYPES IMPLEMENT THEM LATTER
instance Expr ExprT where
    lit :: Integer -> ExprT
    lit x = Lit x
    add :: ExprT -> ExprT -> ExprT
    add x y = Add x y
    mul :: ExprT -> ExprT -> ExprT
    mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Excercise 4

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y
instance Expr Bool where
    lit x = x > 0
    mul x y = x && y
    add x y = x || y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 (x + y `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (x * y `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


    