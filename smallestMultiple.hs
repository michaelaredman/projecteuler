-- Smallest multiple
-- https://projecteuler.net/problem=5
import qualified Data.Map.Strict as M

-- fastest using lcm, but let's do it using prime decomposition for practice

type PrimeCount = M.Map Int Int

-- simplest way to find prime factors
primeFactors :: Int -> [Int]
primeFactors n = do
    case factors of
        [] -> [n]
        _ -> factors ++ primeFactors (n `div` (head factors))
    where
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n-1]

incrementCount :: PrimeCount -> Int -> PrimeCount
incrementCount oldCount prime = M.insertWith (+) prime 1 oldCount

incrementManyCounts :: [Int] -> PrimeCount
incrementManyCounts [] = M.empty
incrementManyCounts (p:ps) = incrementCount (incrementManyCounts ps) p

-- count the number of primes in the prime decomposition of an integer
findFactorCount :: Int -> PrimeCount
findFactorCount n = incrementManyCounts (primeFactors n)

-- update the maximum count of each prime factor for a given prime count
updateOverallCount :: PrimeCount -> PrimeCount -> PrimeCount
updateOverallCount givenPrimeCount overallCount 
    = updateOverallCountList (M.toList givenPrimeCount) overallCount

updateOverallCountList :: [(Int, Int)] -> PrimeCount -> PrimeCount
updateOverallCountList [] overallCount = overallCount
updateOverallCountList ((k, v):pc) overallCount = M.insertWith (max) k v overallCount 

-- update the maximum count of each prime factor using the prime decomp for given integer
updateMinimalFactors :: PrimeCount -> Int -> PrimeCount
updateMinimalFactors oldMinimalFactors x 
   = updateOverallCount (findFactorCount x) oldMinimalFactors 

-- for a list of integers, find the maximum count of each prime factor in each integer
findMinimalFactors :: [Int] -> PrimeCount
findMinimalFactors [] = M.empty
findMinimalFactors (x:xs) = updateMinimalFactors (findMinimalFactors xs) x 

findLCM :: [Int] -> Int
findLCM as = do
    product $ [k ^ v | (k, v) <- minimalFactors]
    where
        minimalFactors = M.toList (findMinimalFactors as)

result = findLCM [1..20]

-- using lcm
alternate = foldl lcm 1 [1..20]




