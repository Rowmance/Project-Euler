-- Problem 016
-- https://projecteuler.net/problem=16

import Common

sumOfDigits :: Integer -> Int
sumOfDigits n = foldr1 (+) $ map charToInt $ show n

-----
f = sumOfDigits $ 2 ^ 1000

--------------------------
main = print f 