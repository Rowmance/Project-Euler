-- Problem 012
-- https://projecteuler.net/problem=12

--------------------------
import Common

countFactors :: Integer -> Int
countFactors n = (2 * length [x | x <- [1..isqrt $ fromInteger n], n `mod` (toInteger x) == 0]) - (boolToInt . isSquare $ n)

isSquare :: (Integral a) => a -> Bool
isSquare x = (fromIntegral . floor . sqrt . fromIntegral $ x) == (sqrt . fromIntegral $ x)

triangles :: [Integer]
triangles = [x * (x + 1) `div` 2 | x <- [1..]]

--------------------------
f = head [x | x <- triangles, (countFactors x) > 500]

--------------------------
main = print f 