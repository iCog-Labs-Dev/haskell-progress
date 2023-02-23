module Anagram (anagramsFor) where
import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\a -> let lowA = map toLower a
                                       lowXs = map toLower xs in
                                    lowA /= lowXs && (sort lowA) == (sort lowXs)) xss
                            
