import qualified Data.Map as Map
data Maybe' a = Nothing' | Just' a deriving Show
data Car =  Car { name :: String, price:: Float, company:: Maybe' String } deriving Show
data Gener a b c = Gener {typeOne :: a, typeTwo :: b, typeThree :: c}

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Satruday | Sunday deriving (Bounded, Enum, Eq, Ord, Show, Read)

data LockerState = Taken | Free deriving(Show, Eq)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp key map = case Map.lookup key map of
    Nothing -> Left $ "Locker number " ++ show key ++" doens't exist"
    Just (state, code) -> if state == Free then Right code
                          else Left $ "Locker number " ++ show key ++ " is already taken"


-- Yes no like in javascript

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where 
    yesno Empty = False
    yesno _ = True

instance YesNo Days where
    yesno Sunday = False
    yesno Satruday = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a->a-> a
yesnoIf yesnoV yesnoT yesnoF = if yesno yesnoV then yesnoT else yesnoF

