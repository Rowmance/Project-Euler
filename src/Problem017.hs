-- Problem 017
-- https://projecteuler.net/problem=17

import Common

f = sum [countLetters $ x | x <- [1..1000]]

--------------------------
main = print f 

--------------------------
countLetters :: Int -> Int
countLetters 1 = length "one"
countLetters 2 = length "two"
countLetters 3 = length "three"
countLetters 4 = length "four"
countLetters 5 = length "five"
countLetters 6 = length "six"
countLetters 7 = length "seven"
countLetters 8 = length "eight"
countLetters 9 = length "nine"
countLetters 10 = length "ten"
countLetters 11 = length "eleven"
countLetters 12 = length "twelve"
countLetters 13 = length "thirteen"
countLetters 14 = length "fourteen"
countLetters 15 = length "fifteen"
countLetters 16 = length "sixteen"
countLetters 17 = length "seventeen"
countLetters 18 = length "eighteen"
countLetters 19 = length "nineteen"
countLetters 20 = length "twenty"
countLetters 30 = length "thirty"
countLetters 40 = length "forty"
countLetters 50 = length "fifty"
countLetters 60 = length "sixty"
countLetters 70 = length "seventy"
countLetters 80 = length "eighty"
countLetters 90 = length "ninety"
countLetters 1000 = length "onethousand"
countLetters n 
            | n < 100 = let mod' = n `mod` 10 in countLetters (n - mod') + countLetters mod'
            | n < 1000 = let mod' = n `mod` 100 in 
              countLetters (n `div` 100) 
              + (length "hundred") 
              + if (mod' > 0) then (length "and" + countLetters mod') else 0