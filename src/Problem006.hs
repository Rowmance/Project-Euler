-- Problem 006
-- https://projecteuler.net/problem=6

sumOfSquares :: Int
sumOfSquares = sum [x * x | x <- [1..100]]

squareOfSums :: Int
squareOfSums = sum' * sum' where
  sum' = sum [1..100]

--------------------------
f :: Int
f = squareOfSums - sumOfSquares

--------------------------
main = print f