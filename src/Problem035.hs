-- Problem 035
-- https://projecteuler.net/problem=35

import Common
import Data.List (nub)

significanceOfFirstDigit :: Int -> Int
significanceOfFirstDigit n = 10^(pow)
  where
    pow = floor $ (log $ fromIntegral $ abs n) / ln10
    ln10 = log 10

countDigits :: Int -> Int
countDigits n = 1 + (floor $ (log $ fromIntegral $ abs n) / ln10)
  where ln10 = log 10

rotate :: Int -> Int
rotate n = (n `div` 10) + (lastDigit * sig)
  where 
    lastDigit = n `mod` 10
    sig = significanceOfFirstDigit n


circularNumbers :: Int -> [Int]
circularNumbers n = fn (countDigits n) n
  where
    fn 0 _ = []
    fn n x = rotated:(fn (n - 1) rotated) where rotated = rotate x

f = length $ nub [x | y <- [1..limit], let ys = circularNumbers y, all isPrime ys, x <- ys] 
  where limit = 1000000

--------------------------
main = print f