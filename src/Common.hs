-- Common functions for use throughout the Euler Problems
module Common where

-- Converts a character to the corresponding Int. eg, '3' -> 3
charToInt :: Char -> Int
charToInt x = fromEnum x - fromEnum '0'

-- Integer square root, rounded down
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- True if the given number is prime
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]

-- The factorial of the given number
factorial :: Int -> Integer
factorial 1 = 1
factorial n = toInteger n * factorial (n - 1)

-- Combinations choose function
choose :: Int -> Int -> Integer
choose n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))

-- Converts True to 1 and False to 0
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1