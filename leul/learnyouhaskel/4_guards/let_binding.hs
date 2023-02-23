factorials :: (Integral a) => a -> [a]
factorials x = [factorial n | n <- [0..x],  let factorial 0 = 1
                                                factorial a = a * factorial (a-1)]