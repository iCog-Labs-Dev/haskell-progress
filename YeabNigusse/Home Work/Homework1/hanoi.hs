type Peg = String
type Move = (Peg, Peg)
{--
  MOVING DISCKS SHOULD FOLLOW THE FOLLOWING RULES
  1 ONLY ONE DISCK CAN BE MOVED AT A TIME
  2 A DISCK CAN BE MOVED IF IT IS UPPERMOST DISCK ON STACK
  3 NO DISCK MAY BE PLACED ON TOP OF A SMMALER DISCK
--}

{--
   Let us define our function
   Integer -shows the number of discks
   peg1 - where the discs found
   peg2  temporary peg used to move discs
   peg3 - where the discs moved
--}


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest aux = hanoi (n-1) src aux dest ++ [(src, dest)] ++ hanoi (n-1) aux dest src
           