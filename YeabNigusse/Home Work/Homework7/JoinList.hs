module JoinList where

data JoinListBasic a = Empty
                       | Single a
                       | Append (JoinListBasic a) (JoinListBasic a)

                       