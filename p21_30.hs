-- Ninety-Nine Haskell Problems
-- Problems 21-30
-- Solutions by Stephen Brennan
-- Problems from: https://wiki.haskell.org/99_questions/21_to_30

-- Problem 21: Insert an element into a given position in a list (1-index).
insertAt            :: a -> [a] -> Int -> [a]
insertAt a l 1      = a:l
insertAt a (x:xs) n = x:insertAt a xs (n-1)

-- Problem 22: Create a list containing a range.
--- Solution 1: List comprehension... duh.
range     :: (Enum a, Ord a, Eq a) => a -> a -> [a]
range x y = [x..y]
--- Solution 2: Recursive.
range'          :: (Enum a, Ord a, Eq a) => a -> a -> [a]
range' x y
    | x > y     = []
    | x == y    = [y]
    | otherwise = x:range' (succ x) y
