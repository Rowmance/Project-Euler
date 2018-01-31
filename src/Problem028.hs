-- Problem 028
-- https://projecteuler.net/problem=28

import Common

-- These are literally squares.

toTopRight :: Int -> Int
toTopRight n = (2 * n + 1)^2

toTopLeft :: Int -> Int
toTopLeft n = (2 * n + 1)^2 - 2 * n

toBottomLeft :: Int -> Int
toBottomLeft n = (2 * n + 1)^2 - 4 * n
  
toBottomRight :: Int -> Int
toBottomRight n = (2 * n + 1)^2 - 6 * n

sumOfDiagonals :: Int -> Int
sumOfDiagonals n = 1 + sum [sumAtLevel x | x <- [1..(n - 1) `div` 2]]
  where sumAtLevel n = (toTopRight n) + (toTopLeft n) + (toBottomRight n) + (toBottomLeft n)

f = sumOfDiagonals 1001

--------------------------
main = print f