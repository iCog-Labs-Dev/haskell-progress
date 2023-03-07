module Hopscotch where

skip :: Int -> [a] -> [a]
skip 0 xs = xs
skip nth xs
  | null xs || null y = []
  | otherwise = head y : skip nth (tail y)
  where
    y = drop nth xs


skips' :: Int -> [a] -> [[a]]
skips' 0 xs = [xs]
skips' r xs = skip r xs : skips' (r - 1) xs


skips :: [a] -> [[a]]
skips xs = reverse $ skips' (length xs - 1) xs