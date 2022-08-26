-- Sum square difference
-- https://projecteuler.net/problem=6

sumOfSquares :: Int -> Int
sumOfSquares n = sum [x * x | x <- [1..n]]

squareOfSum :: Int -> Int
squareOfSum n = (sum [1..n]) ^ 2

result = squareOfSum 100 - sumOfSquares 100