-- Problem 015
-- https://projecteuler.net/problem=15

{-
 - A variant of this was my university interview question.
 - 
 - The problem can be simplified into a choice of how many down (or right) moves are to be made in a total 
 - number of moves.
 - 
 - The problem is symmetrical between choosing the down or right moves.
 - ie, (n + m) C n == (n + m) C m
-}

import Common

-----
f = choose 40 20

--------------------------
main = print f 