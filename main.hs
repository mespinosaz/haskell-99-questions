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

-- Problem 6
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (xs) = ((head xs) == (last xs)) && (isPalindrome (init (tail xs)))

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
whatever [] x = [x]
whatever xs x = if last xs == x then xs else xs ++ [x]
compress (x) = foldl (whatever) [] x

-- Problem 9
pack [] = []
pack (x:xs) = [[x] ++ takeWhile (== x) xs] ++ pack (dropWhile (== x) xs)

-- Problem 10
encode x = map (\x -> (myLength x, head x)) (pack x)
