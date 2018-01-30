-- Problem 019
-- https://projecteuler.net/problem=19

import Common

daysInMonth :: Int -> Int -> Int
daysInMonth 0 _ = 31 -- J
daysInMonth 1 year  -- F
          | year `mod` 100 == 0 && year `mod` 400 > 0 = 28
          | year `mod` 4 == 0 = 29
          | otherwise = 28
daysInMonth 2 _ = 31 -- M
daysInMonth 3 _ = 30 -- A
daysInMonth 4 _ = 31 -- M
daysInMonth 5 _ = 30 -- J
daysInMonth 6 _ = 31 -- J
daysInMonth 7 _ = 31 -- A
daysInMonth 8 _ = 30 -- S
daysInMonth 9 _ = 31 -- O
daysInMonth 10 _ = 30 -- N
daysInMonth 11 _ = 31 -- D
daysInMonth n y = daysInMonth (n `mod` 12) y

-- year -> month -> day on first of month
dayOfWeek :: Int -> Int -> Int
dayOfWeek 1900 0 = 0
dayOfWeek 1900 m = ((daysInMonth (m - 1) 1900) + (dayOfWeek 1900 (m - 1))) `mod` 7
dayOfWeek y m = dayOfWeek 1900 ((y - 1900) * 12 + m)

f = length [(y, m) | y <- [1901..2000], m <- [0..11], let day = dayOfWeek y m, day == 6]

--------------------------
main = print f 