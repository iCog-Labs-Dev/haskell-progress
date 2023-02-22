-- declare
myList = [1, 2, 3, 4]

infinite5 = repeat 5

fourFives = replicate 4 5

-- access
take10FromInfinite = take 10 infinite5

drop10FromIninite = drop 10 infinite5

headOfMyList = head myList

lastOfMyList = last myList

tailsOfMyList = tail myList

initOfMyList = init myList

lastOfInfinite5 = last infinite5

headOfInfinite5 = head infinite5

forthElementOfMyList = myList !! 3

-- ops
newList1 = myList ++ [5] -- concat

newList2 = 1 : myList -- prepend

reversed = reverse myList
myListLength = length myList
