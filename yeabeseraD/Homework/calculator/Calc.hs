import ExprT(ExprT(Lit, Add, Mul))
import Parser(parseExp)
import Data.Typeable
import Data.Maybe

eval :: ExprT -> Integer
eval (Lit x) = x
eval (ExprT.Mul expr1 expr2) = eval expr1 * eval expr2
eval (ExprT.Add expr1 expr2) = eval expr1 + eval expr2

evalStr :: String -> Maybe Integer
evalStr expression = parseEval expr
    where expr = parseExp Lit ExprT.Add ExprT.Mul expression
          parseEval Nothing = Nothing
          parseEval (Just pExpression) = Just $ eval pExpression

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit :: Integer -> ExprT
    lit = Lit

    add :: ExprT -> ExprT -> ExprT
    add = ExprT.Add

    mul :: ExprT -> ExprT -> ExprT
    mul = ExprT.Mul

instance Expr Integer where
    lit :: Integer -> Integer
    lit x = x
    
    add :: Integer -> Integer -> Integer
    add = (+)

    mul :: Integer -> Integer -> Integer
    mul = (*)

instance Expr Bool where
    lit :: Integer -> Bool
    lit x = x > 0

    add :: Bool -> Bool -> Bool
    add = (||)

    mul :: Bool -> Bool -> Bool
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where 
    lit :: Integer -> MinMax
    lit = MinMax

    add :: MinMax -> MinMax -> MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y

    mul :: MinMax -> MinMax -> MinMax
    mul (MinMax x) (MinMax y)= MinMax $ min x y

instance Expr Mod7 where
    lit :: Integer -> Mod7
    lit x
        | x <=6 && x >= 0 = Mod7 x
        | otherwise =Mod7 0
    add :: Mod7 -> Mod7 -> Mod7
    add (Mod7 x) (Mod7 y) =Mod7 $ (x+y) `mod` 7

    mul :: Mod7 -> Mod7 -> Mod7
    mul (Mod7 x) (Mod7 y) =Mod7 $ (x*y) `mod` 7

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp
testBool :: Maybe Bool
testBool = testExp
testMM :: Maybe MinMax
testMM = testExp
testSat :: Maybe Mod7
testSat = testExp

data StackExp = PushI Integer
    | PushB Bool
    | Add
    | Mul
    | And
    | Or
    deriving Show

type Program = [StackExp]

instance Expr Program where
    
    lit ::Typeable a => a -> Program
    lit x = fromMaybe [] (cast x >>= castBool >>= push)

        where castBool :: Typeable a => a -> Maybe Bool
              castBool = cast

              push :: Bool -> Maybe Program
              push x = Just $ if x then [PushB (fromJust $ cast x)] else [PushI (fromJust $ cast x)]
    