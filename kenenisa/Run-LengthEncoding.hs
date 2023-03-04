module RunLength (decode, encode) where

digits :: String
digits = ['0'..'9']

decodeText :: String -> String -> String
decodeText [] _ = []
decodeText (x:xs) acc | x `elem` digits = decodeText xs (x:acc)
                      | not (null acc) = replicate (read (reverse acc) ::Int) x ++ decodeText xs []
                      | otherwise = x : decodeText xs acc

decode :: String -> String
decode encodedText = decodeText encodedText []


encodeText :: String -> Char -> Int -> String
encodeText [] cur 1 = [cur]
encodeText [] cur count = show count ++ [cur]
encodeText (x:xs) cur count | x == cur = encodeText xs cur (count+1)
                            | count > 1 = show count ++ [cur] ++ encodeText xs x 1
                            | otherwise = cur : encodeText xs x 1

encode :: String -> String
encode [] = []
encode text = encodeText text (head text) 0
