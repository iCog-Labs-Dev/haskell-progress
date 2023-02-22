myList = [1 .. 20]

odds = [1, 3 .. 20]

evens = [2, 4 .. 20]

oddLetters = ['a', 'c' .. 'z']

lazy = ['a', 'c' ..]

thetheString = cycle "The-"

thetheCharList = cycle ['T', 'h', 'e', '-']

thetheList = cycle ["The", "-"]

-- -------------Comperhensions ------------------- --

powersOf2 = [x ** 2 | x <- [0 ..]]

powersOf2DivisibleBy3 = [x * x | x <- [0 ..], x `mod` 3 == 0]