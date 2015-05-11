-- Problem 1
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]
