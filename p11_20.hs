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
decodeModified l = concat $ map decodeChar l
    where decodeChar (Single x) = [x]
          decodeChar (Multiple n x) = take n (repeat x)
