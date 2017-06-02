module Utils.Prime (primeGenErt, primeFactors) where

-- Sieve of Eratosthenes. Should be suitable for some problems, but certainly not all
primeGenErt :: [Integer]
-- The sieve is an iterative filter. Starting with p as 2, filter out all numbers from 2p to n, then find the next number after p as the next p, and continue until there is no p available

-- so basically, an upper bound is needed so that the algorithm knows where to stop, but haskell is lazy, so we actually don't need one.
-- we can simply start at 2, yield it as the first item of the list, and then we return the rest of all integers that are not divisible by 2.
-- the next number is the first already filtered number that is not divisible by the previous number.
-- so we return 2, then all integers greater than 2 that aren't divisible by 2.
-- the next number is the first number that isn't divisible by any previous numbers, which is 3.
-- the next number is the first number that isn't divisible by 2 or 3, which is 5.
-- next is not divisible by 2, 3, or 5, which is 7.
-- to make this lazy, we "stack" filter functions onto each other so that they are evaluated in ascending order
-- so basically we return 'head list' for the input list, where the initial list is [2..], followed by 'filter ertFilter tail list'

primeGenErt = 2:(ertFilter 2 [3..])
    where ertFilter p (nextP:list) = nextP:(ertFilter nextP (filter (\x -> x `mod` p /= 0) list))

-- Prime factorization, generates in ascending order
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors num = factor:(primeFactors $ num `quot` factor)
    where factor = head $ filter (\x -> num `mod` x == 0) primeGenErt