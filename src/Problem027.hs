-- Problem 027
-- https://projecteuler.net/problem=27

import Common
import Data.List (maximumBy)
import Data.Ord (comparing)

consecutivePrimes :: [Int] -> Int
consecutivePrimes xs = length $ takeWhile isPrime xs

quadratic :: Int -> Int -> [Int]
quadratic a b = [x * x + a * x + b | x <- [0..]]

-- b has to be prime (or n = 0 not prime). Assuming a is also prime doesn't seem unreasonable.
f = fst $ maximumBy (comparing snd) [(a * b, consecutivePrimes $ quadratic a b) | a <- primes, b <- primes]
  where 
    limit = 1000
    primes = [x | x <- [-limit..limit], isPrime (abs x)]

--------------------------
main = print f