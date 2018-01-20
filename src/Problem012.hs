-- Problem 012
-- https://projecteuler.net/problem=12

--------------------------
countFactors :: Integer -> Int
countFactors n = (2 * length [x | x <- [1..isqrt n], n `mod` x == 0]) - (boolToInt . isSquare $ n)
  where isqrt = floor . sqrt . fromIntegral

isSquare :: (Integral a) => a -> Bool
isSquare x = (fromIntegral . floor . sqrt . fromIntegral $ x) == (sqrt . fromIntegral $ x)

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

triangles :: [Integer]
triangles = [x * (x + 1) `div` 2 | x <- [1..]]

--------------------------
f = head [x | x <- triangles, (countFactors x) > 500]

--------------------------
main = print f 