-- Problem 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] ->[a]
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
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
whatever :: Eq a => [a] -> a -> [a]
whatever [] x = [x]
whatever xs x = if last xs == x then xs else xs ++ [x]
compress :: Eq a => [a] -> [a]
compress (x) = foldl (whatever) [] x

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = [[x] ++ takeWhile (== x) xs] ++ pack (dropWhile (== x) xs)

-- Problem 10
encode x = map (\y -> (myLength y, head y)) (pack x)

-- Problem 11
data CountableElement a = Multiple Int a | Single a
    deriving (Show)
encodeModified :: Eq a => [a] -> [CountableElement a]
encodeModified x = map (\y -> if (myLength y) > 1 then Multiple (myLength y) (head y) else Single (head y)) (pack x)

-- Problem 12
decodeModified :: Eq a => [CountableElement a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = [x] ++ decodeModified xs
decodeModified ((Multiple 0 x):xs) = []
decodeModified ((Multiple n x):xs) = decodeModified [Single x] ++ decodeModified [Multiple (n-1) x] ++ decodeModified xs

-- Problem 14
dupli :: Eq a => [a] -> [a]
dupli [] = []
dupli (x:xs) = [x] ++ [x] ++ dupli xs

-- Problem 15
repli :: Eq a => [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = [x | i <- [1..n]] ++ repli xs n

-- Problem 16
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- Problem 17
split :: Eq a => [a] -> Int -> [[a]]
split xs n = [(take n xs), (drop n xs)]
