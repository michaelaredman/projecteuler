-- Multiples of 3 or 5
-- https://projecteuler.net/problem=1

divisible :: (Integral a) => a -> Bool
divisible a = (a `mod` 3 == 0) || (a `mod` 5 == 0)

multiples :: (Integral a) => Int -> [a]
multiples n = filter (divisible) $ take n [0..]

result = sum $ multiples 1000