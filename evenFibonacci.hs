-- Even Fibonacci numbers
-- https://projecteuler.net/problem=2

fibonacciList :: Int -> [Integer]
fibonacciList n = do
    take n $ fibRecurse 0 1
    where
        fibRecurse a b = a : fibRecurse b (a + b)

result = sum $ filter (even) $ takeWhile (<= 4000000) $ fibonacciList 1000
