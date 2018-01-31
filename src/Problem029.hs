-- Problem 029
-- https://projecteuler.net/problem=29

import Common

sequence' :: Int -> Int -> Int
sequence' amax bmax = length . distinct . sort $ [toInteger a^b | a <- [2..amax], b <- [2..bmax]]

f = sequence' limit limit 
  where limit = 100

--------------------------
main = print f