-- Problem 007
-- https://projecteuler.net/problem=7

-- taken from 003
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where isqrt = floor . sqrt . fromIntegral

--------------------------
f :: Int
f = [x | x <- [2..], isPrime x]!!10000

--------------------------
main = print f