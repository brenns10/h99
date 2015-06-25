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
