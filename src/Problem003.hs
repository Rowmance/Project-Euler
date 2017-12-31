-- Problem 003
-- https://projecteuler.net/problem=3

-- This required a bit of thought - the naive approach of getting all factors and then checking
-- for primeness and getting the max value was too slow.

-- We can work upwards instead: if we start from 2 and work upwards until we find a prime factor, we 
-- can divide by it to obtain a number which is either prime, or has prime factors larger than the prime
-- factors which have already been obtained. If we repeat until the answer is prime, we'll have obtained
-- the largest prime factor.

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where isqrt = floor . sqrt . fromIntegral

smallestPrimeFactor :: Int -> Int
smallestPrimeFactor n = [x | x <- [2..n - 1], n `mod` x == 0, isPrime x]!!0

largestPrimeFactor :: Int -> Int
largestPrimeFactor n
  | isPrime n = n
  | otherwise = largestPrimeFactor $ n `div` smallestPrimeFactor n

--------------------------
f :: Int
f = largestPrimeFactor 600851475143

--------------------------
main = print f