-- Special Pythagorean triplet
-- https://projecteuler.net/problem=9

isPerfectSquare :: Int -> Bool
isPerfectSquare n = do
    n == squaredRoot
    where
        squaredRoot = (floor $ sqrt $ fromIntegral n) ^ 2

validPair :: (Int, Int) -> Bool
validPair (a, b) = do
    isPerfectSquare csquared &&  a + b + c == 1000
    where
        csquared = a^2 + b^2
        c = floor $ sqrt $ fromIntegral csquared

pairs = [(a, b) | a <- [1..1000], b <- [1..1000]]

(a1, b1) = head $ filter validPair pairs
result = a1 * b1 * (floor $ sqrt $ fromIntegral (a1^2 + b1^2))


