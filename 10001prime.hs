-- 10001st prime
-- https://projecteuler.net/problem=7

turner :: [Int]
turner =  sieve [2 .. ]
   where 
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

result = turner!!10000