-- Problem 026
-- https://projecteuler.net/problem=26

import Common
import Data.List (maximumBy)
import Data.Ord (comparing)

-- All non-repeating decimals can be written in the form 1/(2^m * 5^n)
-- therefore reciprocals will repeat for any numbers which have factors other than 2 and 5.
--
-- In addition, the length of a reciprocal of a number with 2 or 5 as factors is the same 
-- as that number /2 or /5 so we can exclude those.
--
-- Every remaining reciprocal repeats, and repeats immediately (ie, there are no non-repeating digits)
shouldInclude :: Int -> Bool
shouldInclude n = n `mod` 5 /= 0 && n `mod` 2 /= 0

-- 9, 99, 999, etc
nines :: [Integer]
nines = [f x | x <- [0..]]
  where 
    f 0 = 9
    f n = (f $ n - 1) * 10 + 9

-- Every repeating decimal can be written as n/9 or n/99 or n/999 etc.
-- Thus we can obtain the length of the repeating element by seeing the first 9.. number
-- which is a multiple of the given number
lengthOfReciprocal :: Int -> Int
lengthOfReciprocal n = snd $ head $ filter (\x -> (fst x) `mod` (toInteger n) == 0) $ zip nines [1..]

f = fst $ maximumBy (comparing snd) [x | x <- zip nos $ map lengthOfReciprocal nos] 
  where 
    limit = 10000
    nos = filter shouldInclude [1..limit]

--------------------------
main = print f