-- Problem 1
myLast (x:[]) = x
myLast (x:xs) = myLast xs
