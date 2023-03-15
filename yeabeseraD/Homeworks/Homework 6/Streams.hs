data Stream a = Stream {value :: a, cons :: Stream a}

instance Show a => Show (Stream a) where
    show stream = take 20 (show $ streamToList stream) 

streamToList :: Stream a -> [a]
streamToList (Stream value tailList) = value:streamToList tailList

streamRepeat :: a -> Stream a
streamRepeat value = Stream value (streamRepeat value)

streamMap :: (a -> b) -> Stream a -> Stream b 
streamMap f (Stream v tailStream) = Stream (f v) (streamMap f tailStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed) )