maxElement :: (Ord a, Num a) => [a] -> a
maxElement lst =
  if not (null (tail lst))
    then max (head lst) (maxElement (tail lst))
    else head lst

minElement :: (Ord a, Num a) => [a] -> a
minElement lst = if not (null (tail lst))
    then min (head lst) (minElement (tail lst))
    else head lst

minMax :: (Ord a, Num a) => [a] -> (a, a)
minMax lst = (minElement lst, maxElement lst)

