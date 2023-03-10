type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disks source destination auxilary = hanoi (disks-1) source auxilary destination ++ [(source, destination)] ++ hanoi (disks-1) auxilary destination source