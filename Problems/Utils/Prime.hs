module Utils.Prime (primeGen, primeGenErt, primeGenSun, primeFactors, isPrime, primes, primeFactors') where


--------------------
-- Prime Generation
--------------------
-- This is a hard problem to solve, so there happen to be many solutions for it, and the faster ones are harder to implement than the slower ones, so we are implementing them one by one.
-- The primeGen function always points to the current fastest algorithm.

primeGen :: (Integral a) => [a]
primeGen = primeGenSun

-------------------------------------------------
-- Sieve of Eratosthenes (slow but simple sieve)
-------------------------------------------------
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

-- ertFilter is a function that takes a p (the previously computed prime) and a list of numbers (all numbers greater than p that are not divisible by primes before p).
-- the first number in the list is always the next prime, so we pluck that off and return it, then we recurse with our new prime as p and the rest of the numbers filtered for divisibles of p.
primeGenErt :: (Integral a) => [a]
primeGenErt = 2:(ertFilter 2 [3..])
    where ertFilter p (nextP:list) = nextP:(ertFilter nextP (filter (\x -> x `mod` p /= 0) list))

------------------------------------------------
-- Sieve of Sundaram (faster than Eratosthenes)
------------------------------------------------
-- This sieve may be more difficult to implement without an upper bound, but we shall try.
-- Pick an upper bound n. For all numbers i and j, where 1 lte i lte j and (i + j + 2ij) lte n, remove (i + j + 2ij) from the list. Take the remaining numbers, multiply them by 2 and add 1. Prepend 2 and you have your list.
-- So basically, the only thing that the upper bound is used for is generating bounded i and j. So we can start with i and j equal to 1, meaning k (number to remove) is (i+j+2ij) which is 4. All numbers less than this number
-- are multiplied by 2 and added to 1, so 1, 2, 3 become 3, 5, and 7.
-- Now we increment j and do the same thing. k is 7, so now we take all numbers between 4 and 7 exclusive and do 2n+1, so we get [5,6] yielding [11,13].
-- Next we increment i until j, so our next k is 12, so now [8,9,10,11] -> [17,19,21,23]. 21 is NOT prime, so we have an issue here.
-- The problem is that incrementing j and then incrementing i from 1 to j doesn't generate ordered numbers. (i+j+2ij) is (i(j+1)+j(i+1)), which doesn't help us...
-- What we can do is increment i or j. Incrementing either will guarantee a larger number than the previous one.
-- So (i,j) as (1,1) is 4
-- We can only increment j now, so (i,j) as (1,2) is 7
-- We can now increment either. (i,j) as (1,3) is 10, and (i,j) as (2,2) is 12, so (1,3) wins.
-- Now we have (1,4) or (2,3), which are 13 and 17 respectively.
-- However, we now skipped (2,2). Let's order the first 20 pairs in order of k:
-- (1,1)4,(1,2)7,(1,3)10,(2,2)12,(1,4)13,(2,3)17,(1,5)16,(1,6)19,(2,4)22,(3,3)24,(2,5)27
-- Ok, so something is emerging. For every fixed i, there is a minimum k and a linear progression with incrementing j.
-- For i as 1, the progression is (1+3j) where j geq 1, so 4,7,10,13,16,19,22,25,...
-- For i as 2, the progression is (2+5j) where j geq 2, so 12,17,22,27,32,37,...
-- For i as 3, the progression is (3+7j) where j geq 3, so 24,33,40,47,54,...
-- For i as 4, the progression is (4+9j) where j geq 4, so 40,49,58,67,...
-- Each i has a starting number that we need to keep track of, and once we cross it, we need to track an additional sequence of k's.
-- So we start with i as 1. We need to know when to start tracking i as 2, so we compute the minimum, which is i as 2,j as 2 which is 12.
-- Once we compute a k gte 12, we need to include the "i as 2" k's, and we need to compute the min k for i as 3, which is 24, and once we hit that we need to track 3 lists.
-- This will gradually take up more memory, but luckily the min k increases exponentially: 4,12,24,40,60,84,112,...

-- TOP LEVEL FUNCTION
-- We start with 2, because the sieve algorithm gives us all odd primes.
-- This function takes the list of k-values and the list of natural numbers n. We filter all k's out of the n's, and each leftover number is put through 2n+1 and emitted.
-- So we pluck a k and an n. If n is less than k, we 2n+1 it and recurse with the same k's list and the remaining n's
-- If n is equal to k, we recurse with the remaining k's and n's, ignoring that n.
-- To generate the k-values, we call kValues.
primeGenSun :: (Integral a) => [a]
primeGenSun = 2:(primeGenSun' kValues [1..])
    where primeGenSun' (k:ks) (n:ns)
            | (n < k) = (2*n+1):(primeGenSun' (k:ks) ns)
            | (n == k) = primeGenSun' ks ns

-- kValues yields every possible k (every i+j+2ij for integers i and j where 1 <= i <= j) in order.
-- To do this it uses a list of lists of ks where each list in the list is every k for a fixed i.
-- Each element of kValues is the next minimum value of all lists, where there should be no duplicates.
-- Each call to getNextK returns the next k-value and the next list of lists of k's. So we yield the next k and recurse with the next list.
kValues :: (Integral a) => [a]
kValues = kValues' listOfListsOfKs
    where kValues' lol = nextK:(kValues' nextLol)
            where (nextK, nextLol) = getNextK lol

-- Get next k will return the minimum k-value and a new list of lists for a given list of lists.
-- Each list in the list is initialized with a -1 at the front, to indicate that no values from that list have been returned yet (a "closed" state).
-- We iterate each list starting at the first list, and pluck the first value off of it.
-- As soon as that first value is a -1, we know that we don't need to look at the next list because each list's first value will always be greater than the previous list's first value.
-- If we are looking at the first two lists and both have been opened, we need to look at the next list to see if it is closed, so we recurse without the first list to get the min value of the remaining lists.
-- What this effectively does is get the minimum value of every opened list, making sure to open the first closed list if it now has values that are smaller than all the preceding lists.
getNextK :: (Integral a) => [[a]] -> (a, [[a]])
getNextK ((k00:k01:k0Stack):(k10:k11:k1Stack):kStacks)
    | (k00 == (-1))               = (k01, k0Stack:(k10:k11:k1Stack):kStacks)        -- first list hasn't been opened, return its first item
    | (k10 == (-1) && k00 < k11)  = (k00, (k01:k0Stack):(k10:k11:k1Stack):kStacks)  -- second list hasn't been opened but first is still less, return the first
    | (k10 == (-1) && k00 == k11) = (k00, (k01:k0Stack):k1Stack:kStacks)            -- second list hasn't been opened and first is equal to second, return it and remove from both
    | (k10 == (-1))               = (k11, (k00:k01:k0Stack):k1Stack:kStacks)        -- second list hasn't been opened and first is greater than second, return second and leave first
    | (k00 < nextMinK)            = (k00, (k01:k0Stack):(k10:k11:k1Stack):kStacks)  -- both lists have been opened and first is less than the minimum of the rest, return the first and leave the rest as it was
    | (k00 == nextMinK)           = (k00, (k01:k0Stack):nextKStacks)                -- both lists opened, first is equal to the min of the rest, return first, keep the new remaining lists
    | otherwise                   = (nextMinK, (k00:k01:k0Stack):nextKStacks)       -- both lists opened, first is greater than the min of the rest, keep first, keep the new remaining lists
    where (nextMinK, nextKStacks) = getNextK ((k10:k11:k1Stack):kStacks)            -- if we need to check the next list, drop the first list and recurse

-- This generates all k-values, literally all of them. It uses the formula i+j+2ij and returns a list of lists where each list has a fixed i value,
-- and contains the k's for every possible value of j, in ascending order.
listOfListsOfKs :: (Integral a) => [[a]]
listOfListsOfKs = listOfListsOfKs' 1
    where listOfListsOfKs' i = (listOfKsForI i):(listOfListsOfKs' (i+1))
          listOfKsForI i = -1:[2*i+2*i*i,2*i+1+2*i*i+2*i..]

--------------------
-- Other prime APIs
--------------------

-- Prime factorization, generates in ascending order
primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors num = factor:(primeFactors $ num `quot` factor)
    where factor = head $ filter (\x -> num `mod` x == 0) primeGenSun

-- primality test, this can almost definitely be improved
isPrime :: (Integral a) => a -> Bool
isPrime num = num > 1 && not (hasFactors num)
    where hasFactors num = any (\x -> num `mod` x == 0) [2..(floor $ sqrt $ fromIntegral num)]

-- naive prime generation, which ironically enough is faster than either sieve
primes :: (Integral a) => [a]
primes = filter isPrime [2..]

-- Prime factorization, generates in ascending order (using faster prime generation)
primeFactors' :: (Integral a) => a -> [a]
primeFactors' 1 = []
primeFactors' num = factor:(primeFactors $ num `quot` factor)
    where factor = head $ filter (\x -> num `mod` x == 0) primes
