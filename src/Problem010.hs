-- Problem 010
-- https://projecteuler.net/problem=10

import Common

--------------------------
f :: Int
f = sum [x | x <- [2..2000000], isPrime x]

--------------------------
main = print f
