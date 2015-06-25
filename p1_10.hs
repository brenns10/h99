-- Ninety-Nine Haskell Problems
-- Problems 1-10
-- Solutions by Stephen Brennan
-- Problems from: https://wiki.haskell.org/99_questions/1_to_10

-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "list is empty"
myLast [x] = x
myLast (_:xs) = myLast xs
