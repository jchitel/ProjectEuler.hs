import Utils.Prime (isPrime)

main = print problem50Value

-- build up the list of primes <1e6 in descending order
primes :: [Int]
primes = filter isPrime [999999,999998..2]

problem50Value :: Int
problem50Value = findLongestSum [head primes] (tail primes) 1 (head primes)

-- cps: consecutive primes that we've built up so far
-- (p:ps): the primes that succeed cps
-- maxlen: the max list length that we've tracked so far
-- max: the max prime sum that we've tracked so far
findLongestSum :: [Int] -> [Int] -> Int -> Int -> Int
findLongestSum [] _ _ max = max
findLongestSum _ [] _ max = max
findLongestSum cps@(cp:rcps) (p:ps) maxlen max
    | s >= 1000000                 = findLongestSum rcps (p:ps) maxlen max     -- if the sum is greater than 1e6, we need to cut off the head of the list and try again
    | len > maxlen && (isPrime s)  = findLongestSum (cps ++ [p]) ps len s      -- if the length of this list is greater than the max and the sum is prime, we have a new max, continue adding primes to the list
    | otherwise                    = findLongestSum (cps ++ [p]) ps maxlen max -- otherwise, add a prime to the list and continue
    where s = sum cps
          len = length cps
