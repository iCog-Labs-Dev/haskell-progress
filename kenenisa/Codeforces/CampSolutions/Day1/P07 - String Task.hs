
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196645941

import Data.Char (toLower)
import Data.List (intersperse)

main :: IO ()
main = do
  s <- getLine
  let vowels = "AEIOUYaeiouy"
  putStrLn $ '.' : intersperse '.' (map toLower (filter (`notElem` vowels) s))
