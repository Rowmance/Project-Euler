-- Problem 002
-- https://projecteuler.net/problem=2

--------------------------
f :: Int
f = sum $ takeWhile (<= 4000000) [x | x <- fibs, even x]

--------------------------
main = print f
