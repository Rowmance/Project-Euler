-- Problem 001 
-- https://projecteuler.net/problem=1

f :: int
f = sum $ [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

--------------------------
main = print f
