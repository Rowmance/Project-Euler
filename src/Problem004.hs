-- Problem 004
-- https://projecteuler.net/problem=4

-- The 999-900 range is assumptive, though seems reasonable given the nature of the question.

isPalindrome :: Int -> Bool
isPalindrome n = (reverse $ str) == str where
  str = show n

--------------------------
f :: Int
f = maxList [a * b | a <- [999,998..900], b <- [999,998..900], isPalindrome $ a * b] where
  maxList = foldr1 max 

--------------------------
main = print f