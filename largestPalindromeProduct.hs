-- Largest palindrome product
-- https://projecteuler.net/problem=4

palindromePair pair = do
    str == reverse str
    where
        str = show $ (fst pair * snd pair)

palindromeNumber = foldl max 1 [x * y | x <- [100..999], y <- [100..999], palindromePair (x, y)]

palindromeNumber2 = maximum [x * y | x <- [100..999], y <- [100..999],
                             x >= y, palindromePair (x, y)]