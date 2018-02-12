-- Problem 034
-- https://projecteuler.net/problem=34

import Common

isSumOfFactorial :: Integer -> Bool
isSumOfFactorial n = n == (foldr1 (+) . map factorial . map charToInt $ show n)

f = sum [x | x <- [3..1000000], isSumOfFactorial x]

--------------------------
main = print f