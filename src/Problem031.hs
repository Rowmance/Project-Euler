-- Problem 031
-- https://projecteuler.net/problem=31

import Common

makeChange :: Int -> [Int] -> [[Int]]
makeChange 0 _ = [[]]
makeChange n coins = foldr1 (++) $ map (\c -> map (c:) $ makeChange (n - c) (filter (<=c) coins)) $ validCoins
  where 
    validCoins = filter (<= n) coins
    

f = length $ makeChange 200 [1, 2, 5, 10, 20, 50, 100, 200]

--------------------------
main = print f