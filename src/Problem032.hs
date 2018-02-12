-- Problem 032
-- https://projecteuler.net/problem=32

import Data.List (sort, nub)

isPandigitalSet :: Int -> Int -> Int -> Bool
isPandigitalSet a b c = "123456789" == (sort $ (show a) ++ (show b) ++ (show c))

f = sum $ nub [c | a <- [1..limit], b <- [a..limit], let c = a * b , isPandigitalSet a b c]
  where limit = 2000

--------------------------
main = print f