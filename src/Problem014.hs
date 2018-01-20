-- Problem 014
-- https://projecteuler.net/problem=14

--------------------------
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n 
      | n `mod` 2 == 0 = n : (collatz $ n `div` 2)
      | otherwise = n : (collatz $ (3 * n) + 1)
      
biggestSnd :: (Int, Int) -> (Int, Int) -> (Int, Int)
biggestSnd a b 
      | snd a > snd b = a
      | otherwise = b

-----
f = fst $ foldr1 biggestSnd [(x, length . collatz $ x) | x <-[999999, 999998..1]]

--------------------------
main = print f 