-- Ninety-Nine Haskell Problems
-- Problems 1-10
-- Solutions by Stephen Brennan
-- Problems from: https://wiki.haskell.org/99_questions/1_to_10

-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "list is empty"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2: Find the last but one element of a list.
--- Solution 1: the probably expected solution, by cases
myButLast :: [a] -> a
myButLast [] = error "list is empty"
myButLast [x] = error "list has just one element"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs
--- Solution 2: by chaining (inspired by similar solution for Problem 1)
myButLast' = head . tail . reverse


-- Problem 3: Find the kth element of a list (indexed starting at 1).
elementAt :: (Integral a) => [b] -> a -> b
elementAt _ x
    | x <= 0 = error "negative index"
elementAt [] _ = error "index out of range"
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

-- Problem 4: Find the number of elements in a list.
--- Solution 1: expected recursive function
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
--- Solution 2: sum of list comprehension
myLength' l = sum [1 | _ <- l]
--- Solution 3: sum of mapping
myLength'' = sum . map (\_ -> 1)
--- Solution 4: fold
myLength''' :: [a] -> Int
myLength''' = foldl (\acc _ -> acc + 1) 0

-- Problem 5: Reverse a list.
--- Solution 1: Naive
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
--- Solution 2: Continuation (mostly just for fun from Scheme)
myReverse' :: [a] -> [a]
myReverse' l = myReverse_cps l id
    where myReverse_cps [] ret = ret []
          myReverse_cps (x:xs) ret = myReverse_cps xs (\v -> ret (v ++ [x]))
--- Solution 3: fold cons
myReverse'' :: [a] -> [a]
myReverse'' = foldl (\acc x -> x:acc) []

-- Problem 6: Find out whether a list is a palindrome.
--- Solution 1: A somewhat lengthy one that tests pairs.  Not very good.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == lst = isPalindrome middle
    | otherwise = False
    where lst = last xs
          middle = init xs
--- Solution 2: Check whether the reversed is equal to the original.
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' l = and $ map (\(a,b) -> a == b) (zip l (reverse l))

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8: Eliminate consecutive duplicates.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = x:compress (y:xs)

-- Problem 9: Pack consecutive duplicates into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l = bin:pack rem
    where (bin,rem) = span (== head l) l

-- Problem 10: Run-length encoding.
encode :: (Eq b) => [b] -> [(Int,b)]
encode l = map (\x -> (length x, head x)) (pack l)
