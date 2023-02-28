import Data.Char (chr, ord)
encode :: Int -> String -> String 
encode n =  map (chr . (+n) . ord)

decode :: Int -> String -> String
decode n = encode $ negate n 


--Doesn't work
encodeNTimes :: Int -> String -> String
encodeNTimes 0 xs = xs
encodeNTimes n xs = encodeNTimes (n-1) (encode n xs)

decodeNTimes :: Int -> String -> String
decodeNTimes = encodeNTimes . negate