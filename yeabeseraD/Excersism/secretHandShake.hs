module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
    | not (null handshakeMessage) && last handshakeMessage == "reverse" = reverse $ init handshakeMessage
    | otherwise = handshakeMessage
    where secretMessage = reverse $ extractCode $ reverse $ toBinary n
          filteredMessage = if head secretMessage == [0] then tail secretMessage else secretMessage
          handshakeMessage
            | not $ null filteredMessage = map pickAction filteredMessage
            | otherwise = []

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary 1 = [1]
toBinary x = x `mod` 2 : toBinary (x `div` 2)

pickAction:: [Int] -> String
pickAction [1] = "wink"
pickAction [1,0] = "double blink"
pickAction [1,0,0] = "close your eyes"
pickAction [1,0,0,0] = "jump"
pickAction [1,0,0,0,0] = "reverse"

extractCode :: [Int] -> [[Int]]
extractCode [0] = [[0]]
extractCode [1] = [[1]]
extractCode (1:xs) = (1: map (\_ -> 0) xs) : extractCode xs
extractCode (0:xs) = extractCode xs