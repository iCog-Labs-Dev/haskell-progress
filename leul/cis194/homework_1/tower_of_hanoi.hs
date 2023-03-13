-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

type Peg = String
type Move = (String, String)
pr

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi 2 a b c = [(a, c), (a, b), (c, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a