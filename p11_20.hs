-- Ninety-Nine Haskell Problems
-- Problems 11-20
-- Solutions by Stephen Brennan
-- Problems from: https://wiki.haskell.org/99_questions/11_to_20

-- Problem 11: Modified run-length encoding. (if there is only one instance,
-- just include the character, not the count)
data RunLength a = Single a | Multiple Int a deriving Show
encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified l@(x:_) = (if length bin == 1 then Single x
                          else Multiple (length bin) x):encodeModified rem
    where (bin,rem) = span (== x) l
encodeModified [] = []

-- Problem 12: Decode modified run-length encoding from problem 11.
decodeModified :: [RunLength a] -> [a]
decodeModified l = concatMap decodeChar l
    where decodeChar (Single x) = [x]
          decodeChar (Multiple n x) = replicate n x

-- Problem 13: Run length encoding "direct" (without creating sublists).  I kind
-- of think my solution to 11 already fits the bill.  Sort of.  There's no step
-- where I have a list of sublists.  However, I stil do use span to create a
-- sublist.  So I'll make a nicer implementation.
encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect = foldr helper []
    where helper x acc@((Single y):ys) = if x == y then (Multiple 2 y):ys
                                         else (Single x):acc
          helper x acc@((Multiple n y):ys) = if x == y then (Multiple (n + 1) y):ys
                                             else (Single x):acc
          helper x [] = [Single x]

-- Problem 14: Duplicate elements in a list: [1,2,3]->[1,1,2,2,3,3].
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15: Replicate the elements in a list the given number of times.
repli :: [a] -> Int -> [a]
repli l n = concatMap (replicate n) l

-- Problem 16: Drop every nth element of a list.
dropEvery :: [a] -> Int -> [a]
dropEvery l n = map snd $ filter (\(m,x) -> m/=n) $ zip (cycle [1..n]) l

-- Problem 17: Split a list into two parts, where length of first part is given.
--- Solution 1: Recursive
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split l n
    | n <= 0 = ([], l)
split (x:xs) n = ((x:h),r)
    where (h,r) = split xs (n-1)
--- Solution 2: take+drop
split' :: [a] -> Int -> ([a], [a])
split' l n = (take n l, drop n l)

-- Problem 18: Extract a slice from a list, including both indices, start
-- indexing at 1.
--- Solution 1: take/drop
slice :: [a] -> Int -> Int -> [a]
slice list start stop = take (stop-start+1) (drop (start-1) list)
--- Solution 2: recursive
slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' l@(x:xs) start stop
    | stop < 1   = []
    | start <= 1 = x:(slice' xs 0 (stop-1))
    | otherwise  = slice' xs (start-1) (stop-1)
