-- Problem 021
-- https://projecteuler.net/problem=21

import Common

d :: Int -> Int
d 1 = 0
d n = sum . properDivisors $ n

isAmicable :: Int -> Bool
isAmicable x = x == (d dx) && x /= dx where dx = d x

f = sum [x | x <- [2..9999], isAmicable x]

--------------------------
main = print f 