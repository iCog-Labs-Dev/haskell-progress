--Chapter 13 For a Few Monads More 
isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compared gang size to 9.")

-- let us write a function that take a value with a context and isBigGnag and return Bool with combined context

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

{--ghci> (3, "Smallish gang.") `applyLog` isBigGang
(False,"Smallish gang.Compared gang size to 9")
ghci> (30, "A freaking platoon.") `applyLog` isBigGang
(True,"A freaking platoon.Compared gang size to 9")
--}

--Monoids to the rescue
