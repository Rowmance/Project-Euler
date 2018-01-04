-- Problem 010
-- https://projecteuler.net/problem=10

-- taken from 003
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where isqrt = floor . sqrt . fromIntegral

--------------------------
f :: Int
f = sum [x | x <- [2..2000000], isPrime x]

--------------------------
main = print f
