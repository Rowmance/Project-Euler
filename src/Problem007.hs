-- Problem 007
-- https://projecteuler.net/problem=7

import Common

--------------------------
f :: Int
f = [x | x <- [2..], isPrime x]!!10000

--------------------------
main = print f