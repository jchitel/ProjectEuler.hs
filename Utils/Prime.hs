module Utils.Prime (primeGenErt) where

-- Sieve of Eratosthenes. Should be suitable for some problems, but certainly not all
primeGenErt :: Integer -> [Integer]
-- The sieve is an iterative filter. Starting with p as 2, filter out all numbers from 2p to n, then find the next number after p as the next p, and continue until there is no p available
primeGenErt max = ertFilter 2 [3..max]
    where ertFilter _ [] = []
          ertFilter p list = p:(ertFilter nextP nextList)
              where (nextP:nextList) = filter (\x -> x `mod` p /= 0) list
