import Utils.Prime (isPrime)

main = print problem47Value

type I = Integer

problem47Value :: I
problem47Value = findDistinct 210 210 0 primes 1 3 -- 210 is the smallest number with 4 unique factors

-- for low enough numbers this is faster than a sieve
primes :: [I]
primes = filter isPrime [2..]

-- From a starting number, find the first number of a consecutive sequence of 4 where each number has 4 unique prime factors.
-- Args:
-- n: the number we are checking
-- n': the factoring result so far, starting with n and decreasing to 1
-- f: the number of distinct factors found so far, starting with 0
-- (p:ps): the current list of primes being checked as factors, starting with the full list of primes
-- lastp: the last prime that was successfully factored, starting with 1
-- nn: the number of remaining numbers to check, starting with 3
findDistinct :: I -> I -> I -> [I] -> I -> I -> I
findDistinct n n' f (p:ps) lastp nn
    | n' == 1 && f /= 4               = next 3      -- not 4 distinct factors, try the next number, resetting the remaining numbers to 3
    | n' == 1 && f == 4 && nn == 0    = (n-3)       -- success! return the first in the sequence
    | n' == 1 && f == 4               = next (nn-1) -- has 4 distinct factors, try the next number with (nn-1) remaining to find
    | f >= 4                          = next 3
    | r /= 0                          = current n' f     ps     lastp -- not divisible by this prime, try the next one
    | r == 0 && p == lastp            = current q  f     (p:ps) lastp -- divisible but we've seen it already, divide the number and keep everything else the same
    | otherwise                       = current q  (f+1) (p:ps) p     -- new divisible prime, increment the factors and save this prime
    where (q, r) = n' `quotRem` p
          current n'' f' ps' lastp' = findDistinct n     n''   f' ps'    lastp' nn  -- continue with the current n and nn
          next nn'                  = findDistinct (n+1) (n+1) 0  primes 1      nn' -- try the next number with a new nn
