-- Problem 1
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
