-- Largest prime factor
-- https://projecteuler.net/problem=3
import Data.List -- :m + Data.List

isSquare :: Integer -> Bool
isSquare n = do
    n == squareofroot
    where
        squareofroot = (floor $ sqrt $ fromIntegral n) ^ 2

fermatFactor :: Int -> Maybe Int
fermatFactor n = do
    find isSquare $ map (\x -> x^2 - n) as
    where
        as = [ceiling $ sqrt n..n]

largestDivisor :: Int -> Maybe Int
largestDivisor n = do
    find (\x -> n `mod` x == 0) [start, start-1..1]
    where
        start = floor $ sqrt $ fromIntegral n

primeFactors :: Int -> [Int]
primeFactors n = do
    case factors of
        [] -> [n]
        _ -> factors ++ primeFactors (n `div` (head factors))
    where
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n-1]

result = last $ primeFactors 600851475143
