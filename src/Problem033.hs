-- Problem 033
-- https://projecteuler.net/problem=33

import Data.List (sort, nub)

f = foldr1 (\(a0, b0) (a1, b1) -> (a0 * a1, b0 * b1)) [(num, den) | 
  num <- [1..9],
  den <- [1..9],
  c <- [1..9],
  num < den, 
  let newNum = num * 10 + c,
  let newDen = c * 10 + den,
  num/den == newNum/newDen]

--------------------------
main = print f