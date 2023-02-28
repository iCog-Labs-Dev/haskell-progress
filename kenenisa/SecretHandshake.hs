module SecretHandshake (handshake) where

handshakes :: [String]
handshakes = ["wink","double blink","close your eyes","jump"]

toBinary :: Int -> Int-> [String]
toBinary _ (-1) = []
toBinary 0 _ = []
toBinary n x | tb = (handshakes !! x) : toBinary (n `mod` (2^x)) (x-1)
             | otherwise = toBinary (n `mod` (2^x)) (x-1)
      where tb = (n `div` (2^x)) > 0 



handshake :: Int -> [String]
handshake n | n > 16 = convert
            | otherwise = reverse convert
        where convert = toBinary (n `mod` 16) 3
