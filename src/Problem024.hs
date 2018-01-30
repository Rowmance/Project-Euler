-- Problem 024
-- https://projecteuler.net/problem=24

import Common

-- Permutations of 0-9 are just numbers greater than or equal to 123456789
-- with all unique digits. 
validNumber :: Int -> Bool
validNumber n = sorted == "0123456789" || sorted == "123456789"
  where sorted = sort $ show n

f = [x | x <- [123456789..], validNumber x]!!999999

--------------------------
main = print f 
