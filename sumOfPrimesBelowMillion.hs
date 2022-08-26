-- Summation of primes
-- https://projecteuler.net/problem=10
import qualified Data.Map as M

turner :: [Int]
turner =  sieve' [2 .. ]
   where 
    sieve' (p:xs) = p : sieve' [x | x <- xs, x `mod` p /= 0]

sieve :: [Int] -> [Int]
sieve xs = paritalSieve xs M.empty
    where
        paritalSieve :: [Int] -> M.Map Int [Int] -> [Int]
        paritalSieve [] table = []
        paritalSieve (x:xs) table = 
            case M.lookup x table of
                Nothing -> x : paritalSieve xs (M.insert (x*x) [x] table)
                Just factors -> paritalSieve xs (foldl reinsert (M.delete x table) factors)
            where
                reinsert table prime = M.insertWith (++) (x+prime) [prime] table

result = sum $ sieve [2..2000000] 

