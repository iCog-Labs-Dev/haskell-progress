import qualified Data.Map as Map
data Maybe' a = Nothing' | Just' a deriving Show
data Car =  Car { name :: String, price:: Float, company:: Maybe' String } deriving Show
data Gener a b c = Gener {typeOne :: a, typeTwo :: b, typeThree :: c}

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Satruday | Sunday deriving (Bounded, Enum, Eq, Ord, Show, Read)

data LockerState = Taken | Free deriving(Show, Eq)

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