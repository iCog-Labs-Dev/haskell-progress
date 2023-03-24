data Stream a = Stream {value :: a, cons :: Stream a}

instance Show a => Show (Stream a) where
    show stream = show (take 20 (streamToList stream)) ++ "..."

streamToList :: Stream a -> [a]
streamToList (Stream value tailList) = value:streamToList tailList

streamRepeat :: a -> Stream a
streamRepeat value = Stream value (streamRepeat value)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream v tailStream) = Stream (f v) (streamMap f tailStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed) )

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (\x -> if odd x then 0 else count2s  x 0) numbers

numbers :: Stream Integer
numbers = streamFromSeed (+1) 1

count2s :: Integer -> Integer -> Integer
count2s 0 acc = acc
count2s 1 acc = acc
count2s x acc
            | even x = count2s (x `div` 2) (acc + 1)
            | otherwise = acc