import Graphics.Win32 (beginDeferWindowPos)
type Bird = Int
type Pole = (Bird, Bird)


leftLand :: Bird -> Pole -> Maybe Pole
leftLand n (left, right)
    | abs (left + n - right) <= 3 = Just (left + n, right)
    | otherwise = Nothing


rightLand :: Bird -> Pole -> Maybe Pole
rightLand n (left, right)
    | abs (right + n - left) <= 3 = Just (left, right + n)
    | otherwise = Nothing


routine :: Maybe Pole
routine = do
            let begin = (0, 0)
            first <- leftLand 1 begin
            second <- leftLand 3 begin
            third <- Nothing
            rightLand 1 first
            
            

