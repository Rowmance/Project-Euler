-- Problem 025
-- https://projecteuler.net/problem=25

import Common

containsAtLeastDigits :: Int -> Integer -> Bool
containsAtLeastDigits n x = x > 10 ^ (n - 1)

f = head [x | x <- zip fibs [0..], containsAtLeastDigits 1000 (fst x)]

--------------------------
main = print f 
