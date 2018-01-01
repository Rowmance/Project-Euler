-- Problem 005
-- https://projecteuler.net/problem=5

-- Didn't need any code for this one, just listed the prime factors of numbers up to 20
-- and eliminated the factors which were in common from previous numbers. 
-- If a number had a prime factor appearing multiple times, it had to appear a minimum of that number
-- of times.

-- the resulting prime factors were as follows.
f :: Int
f = foldr1 (*) [2, 3, 4, 5, 6, 7, 11, 13, 17, 19]

--------------------------
main = print f