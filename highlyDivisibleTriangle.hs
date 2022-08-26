-- Highly divisible triangular number
-- https://projecteuler.net/problem=12
import Math.NumberTheory.Primes.Factorisation

triangleNumbers :: [Int]
triangleNumbers = scanl1 (+) [1..]

primeFactors :: Int -> [Int]
primeFactors n = do
    case factors of
        [] -> [n]
        _ -> factors ++ primeFactors (n `div` (head factors))
    where
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n-1]

numFactors' :: Int -> Int
numFactors' n = 2 ^ (length $ primeFactors n)

result = head $ filter (\x -> (numFactors x) >= 500) triangleNumbers


