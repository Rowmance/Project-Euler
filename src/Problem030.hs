-- Problem 030
-- https://projecteuler.net/problem=30

import Common

isSumOfPowers :: Int -> Int -> Bool
isSumOfPowers n x = (sum $ map (^n) $ map charToInt $ show x) == x

f = sum [x | x <- [2..999999], isSumOfPowers 5 x]

--------------------------
main = print f