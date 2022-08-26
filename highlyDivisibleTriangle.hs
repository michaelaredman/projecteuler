-- Highly divisible triangular number
-- https://projecteuler.net/problem=12

triangleNumbers :: [Int]
triangleNumbers = scanl1 (+) [1..]

primeFactors :: Int -> [Int]
primeFactors n = do
    case factors of
        [] -> [n]
        _ -> factors ++ primeFactors (n `div` (head factors))
    where
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n-1]

result = head $ filter (\x -> (length $ primeFactors x) >= 500) triangleNumbers
