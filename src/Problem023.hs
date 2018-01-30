-- Problem 023
-- https://projecteuler.net/problem=23

import Common
import Debug.Trace
-- first problem in which linked lists alone won't do the trick...
import Data.Set (Set, fromList, member)

isAbundant :: Int -> Bool
isAbundant n = (sum $ properDivisors n) > n

abundants :: [Int]
abundants = [x | x <- [12..], isAbundant x]

-- elem is O(n). Set.member is O(log n)
isSummable :: Set Int -> Int -> Bool
isSummable xs n = any (\x -> member (n - x) xs) xs

f = (sum [1..limit]) - abundantSum
  where 
    limit = 28123
    nos = fromList $ takeWhile (< limit) abundants
    isSummable' = trace (show nos) $ isSummable nos
    abundantSummables = filter isSummable' [1..limit]
    abundantSum = sum abundantSummables

------------------------
main = print f 
