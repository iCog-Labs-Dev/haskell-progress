
myCompare :: Ord a => a -> a -> Char
myCompare x y =
  if (x < y)
    then 'L'
    else
      if (x > y)
        then 'G'
        else 'E'