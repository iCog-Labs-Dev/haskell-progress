safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeLast :: [a] -> Maybe [a]
safeLast [] = Nothing
safeLast lst
          | length lst == 1 = Just []
          | otherwise = Just (tail lst)

safeTail :: [a] -> Maybe a
safeTail[] = Nothing
safeTail lst@(x:xs)
          | length lst == 1 = Just []
          | otherwise = Just xs


