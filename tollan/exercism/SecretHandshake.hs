module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
    | n > 15 = reverse $ handshake (n - 16)
    | n > 7 = handshake (n - 8) ++ ["jump"]
    | n > 3 = handshake (n - 4) ++ ["close your eyes"]
    | n > 1 = handshake (n - 2) ++ ["double blink"]
    | n > 0 = ["wink"]
    | otherwise = []