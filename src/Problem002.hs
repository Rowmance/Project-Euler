-- Problem 002
-- https://projecteuler.net/problem=2

-- https://wiki.haskell.org/The_Fibonacci_sequence
-- this seemed like the most elegant of definitions and is suited for purpose
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--------------------------
f :: Int
f = sum $ takeWhile (<= 4000000) [x | x <- fibs, even x]

--------------------------
main = print f
