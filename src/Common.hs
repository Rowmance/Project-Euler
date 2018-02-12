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
isPrime n
  | n < 2 = False
  | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]

-- The factorial of the given number
factorial :: Int -> Integer
factorial 0 = 1
factorial n = foldr1 (*) [1..toInteger n]

-- Combinations choose function
choose :: Int -> Int -> Integer
choose n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))

-- Converts True to 1 and False to 0
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

-- Returns the sum of the digits in the given int
sumOfDigits :: Integer -> Int
sumOfDigits n = sum $ map charToInt $ show n

-- 'proper divisors'; factors of n less than n
properDivisors :: Int -> [Int]
properDivisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

-- all divisors, including n
divisors :: Int -> [Int]
divisors n = n:properDivisors n

-- Only prime and proper divisors
primeProperDivisors :: Int -> [Int]
primeProperDivisors n = filter isPrime $ properDivisors n

-- Quicksort
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort lesser ++ [x] ++ sort greater
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs

-- The fibbonaci sequence
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Returns the distinct elements in a sorted set
distinct :: Eq a => [a] -> [a]
distinct (x:[]) = [x]
distinct (x0:x1:xs)
       | x0 == x1 = distinct (x1:xs)
       | otherwise = x0:distinct(x1:xs)
