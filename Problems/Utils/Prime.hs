module Utils.Prime (primeGen, primeGenErt, primeGenSun, primeFactors) where

import Debug.Trace

--------------------
-- Prime Generation
--------------------
-- This is a hard problem to solve, so there happen to be many solutions for it, and the faster ones are harder to implement than the slower ones, so we are implementing them one by one.
-- The primeGen function always points to the current fastest algorithm.

primeGen :: [Integer]
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
primeGenErt :: [Integer]
primeGenErt = 2:(ertFilter 2 [3..])
    where ertFilter p (nextP:list) = nextP:(ertFilter nextP (filter (\x -> x `mod` p /= 0) list))

------------------------------------------------
-- Sieve of Sundaram (faster than Eratosthenes)
------------------------------------------------
-- This sieve may be more difficult to implement without an upper bound, but we shall try.
-- Pick an upper bound n. For all numbers i and j, where 1 leq i leq j and (i + j + 2ij) leq n, remove (i + j + 2ij) from the list. Take the remaining numbers, multiply them by 2 and add 1. Prepend 2 and you have your list.
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
-- Once we compute a k geq 12, we need to include the i as 2 k's, and we need to compute the min k for i as 3, which is 24, and once we hit that we need to track 3 lists.
-- This will gradually take up more memory, but luckily the min k increases exponentially: 4,12,24,40,60,84,112,...

-- Now that we have a strategy, let's figure this out.
-- Start with i as 1. Compute the k progression for this (1+3j for all j geq 1) and the min k for i as 2 (12). Pop the first k off the stack. All numbers from the previous k to that k are put through 2n+1 and emitted.
-- If a k is greater than or equal to the next min k, we need to loop that one in. Compute the next k for all current k stacks, pop the min(s), and compute the next primes. Every time we emit primes, we need to hang on to the previous k.
-- Each k stack will always hold the next k at the top, which is great. However, we will need to regenerate the list of stacks for every iteration.

-- primeGenSun' maintains the state of the generation process.
-- It takes 3 parameters:
-- - kStacks (a list of k progressions to pull k values from),
-- - prevK (the previous k value so that we know where the previous chunk of primes left off), and
-- - nextMinK (the minimum of the next unused k progression so that we know when to pull the next one in)
-- We first generate our current primes, which are computed from the exclusive range between the previous k and the current k.
-- Then we prepend that to the next primes (called recursively), where we pass the next values of the parameters.
-- To compute the current k value and the next parameters, we call processKValues.

-- So effectively, we need 2, followed by the sequence of 2*n+1 for every n not in ks, where ks is every k such that (i+j+2ij) exists for some natural numbers i and j where 1 <= i <= j.
-- primeGenSun' takes the list of ks and the list of natural numbers and yields 2*n+1 for every n not in ks.
primeGenSun :: [Integer]
primeGenSun = 2:(primeGenSun' kValues [1..])
    where primeGenSun' (k:ks) (n:ns)
            | (n < k) = (2*n+1):(primeGenSun' (k:ks) ns)
            | (n == k) = primeGenSun' ks ns

-- kValues yields every possible k (every i+j+2ij for integers i and j where 1 <= i <= j) in order.
-- To do this it uses a list of lists of ks where each list in the list is every k for a fixed i.
-- Each element of kValues is the next minimum value of all lists, where there are no duplicates.
-- To prevent infinite recursion, we use the fact that each list's first value is higher than the previous one's, and place a -1 at the start of each list.
-- When we are enumerating all of the lists, as soon as we hit one where the first value is a -1, we check the next value.
-- If that value is lower than the current minimum k in the iteration, we "open" that list and return the new value as the minimum, the next k.
-- This series will get slower as it goes on, but at an exponential rate, because the minimum k for an i increases exponentially as i increases.

-- So...
-- The concept here is that we have lists of lists of k values, where each list is every k for a fixed i.
-- Every list is initialized with a -1 at the front to indicate that it hasn't been "opened" yet.
-- This means that the actual "first" value of a list might be at the second position, so we need to grab the first two elements from each list.
-- We also always need to be looking at the next list, because its first value may be less than or equal to the first value of the current list.
-- If we see that the next list hasn't been opened yet and its first value is greater than the current value, we can guarantee that
-- we don't need to check any more lists

-- alright, this should do it, we're really close
-- basically, when we pass a list of stacks into kValues' we process it and return the sorted list. but there are cases where we want to do this and we only care about the first entry.
-- so what we need to do is revise kValues' to call a new function getNextK, which will take a list of lists of ks and return the next k plus the 
kValues :: [Integer]
kValues = kValues' listOfListsOfKs
    where kValues' lol = nextK:(kValues' nextLol)
            where (nextK, nextLol) = getNextK lol

getNextK :: [[Integer]] -> (Integer, [[Integer]])
getNextK ((k00:k01:k0Stack):(k10:k11:k1Stack):kStacks)
    | (k00 == (-1)) = k01:(kValues' (k0Stack:(k10:k11:k1Stack):kStacks)) -- first list hasn't been opened, return its first item
    | (k00 /= (-1) && k10 == (-1) && k00 < k11) = k00:(kValues' ((k01:k0Stack):(k10:k11:k1Stack):kStacks)) -- second list hasn't been opened but first is still less, return the first
    | (k00 /= (-1) && k10 == (-1) && k00 == k11) = k00:(kValues' ((k01:k0Stack):k1Stack:kStacks)) -- second list hasn't been opened and first is equal to second, return it and remove from both
    | (k00 /= (-1) && k10 == (-1) && k00 > k11) = k11:(kValues' ((k00:k01:k0Stack):k1Stack:kStacks)) -- second list hasn't been opened and first is greater than second, return second and leave first
    | (k00 /= (-1) && k10 /= (-1) && k00 < nextMinK) = k00:(kValues' ((k01:k0Stack):(k10:k11:k1Stack):kStacks)) -- both lists have been opened and first is equal to absolute min, return the first
    | (k00 /= (-1) && k10 /= (-1) && k00 == nextMinK) = k00:(kValues' nextKs) -- this is definitely WRONG, we need to somehow access the GRID that was used in the recursive call
    | otherwise = nextMinK:(kValues' ((k00:k01:k0Stack):nextKs)) -- otherwise return absolute min and leave first and second
    where (nextMinK:nextKs) = kValues' ((k10:k11:k1Stack):kStacks)

kValues = kValues' listOfListsOfKs
    where kValues' ((k00:k01:k0Stack):(k10:k11:k1Stack):kStacks)
           | (k00 == (-1)) = k01:(kValues' (k0Stack:(k10:k11:k1Stack):kStacks)) -- first list hasn't been opened, return its first item
           | (k00 /= (-1) && k10 == (-1) && k00 < k11) = k00:(kValues' ((k01:k0Stack):(k10:k11:k1Stack):kStacks)) -- second list hasn't been opened but first is still less, return the first
           | (k00 /= (-1) && k10 == (-1) && k00 == k11) = k00:(kValues' ((k01:k0Stack):k1Stack:kStacks)) -- second list hasn't been opened and first is equal to second, return it and remove from both
           | (k00 /= (-1) && k10 == (-1) && k00 > k11) = k11:(kValues' ((k00:k01:k0Stack):k1Stack:kStacks)) -- second list hasn't been opened and first is greater than second, return second and leave first
           | (k00 /= (-1) && k10 /= (-1) && k00 < nextMinK) = k00:(kValues' ((k01:k0Stack):(k10:k11:k1Stack):kStacks)) -- both lists have been opened and first is equal to absolute min, return the first
           | (k00 /= (-1) && k10 /= (-1) && k00 == nextMinK) = k00:(kValues' nextKs) -- this is definitely WRONG, we need to somehow access the GRID that was used in the recursive call
           | otherwise = nextMinK:(kValues' ((k00:k01:k0Stack):nextKs)) -- otherwise return absolute min and leave first and second
           where (nextMinK:nextKs) = kValues' ((k10:k11:k1Stack):kStacks)

listOfListsOfKs :: [[Integer]]
listOfListsOfKs = listOfListsOfKs' 1
    where listOfListsOfKs' i = (listOfKsForI i):(listOfListsOfKs' (i+1))
          listOfKsForI i = -1:[2*i+2*i*i,2*i+1+2*i*i+2*i..]




primeGenSun = 2:(primeGenSun' [kProgression 1] 0 (minK 2))
--    where primeGenSun' kStacks prevK nextMinK = (map computeOddPrime [(prevK+1)..(thisK-1)]) ++ (primeGenSun' nextKStacks thisK nextNextMinK)
--            where (nextKStacks, thisK, nextNextMinK) = processKValues kStacks nextMinK

primeGenSun' :: [[Integer]] -> Integer -> Integer -> [Integer]
primeGenSun' kStacks prevK nextMinK = (map computeOddPrime [(prevK+1)..(thisK-1)]) ++ (primeGenSun' nextKStacks thisK nextNextMinK)
    where (nextKStacks, thisK, nextNextMinK) = trace ("Calling processKValues with "++(show $ length kStacks)++" k-stacks and "++(show nextMinK)++" as the next min-k.") $ processKValues kStacks nextMinK

-- processKValues takes our current k stacks and the min k of the next uncomputed stack and computes the current k, the new stacks, and the min k of the next next uncomputed stack
-- the current k is the minimum head value of all the stacks.
-- any stack whose head is the minimum will have that head removed.
-- if the current k is greater than or equal to the nextMinK value, we need to compute the next stack and add it to the list.
processKValues :: [[Integer]] -> Integer -> ([[Integer]], Integer, Integer)
processKValues kStacks nextMinK = trace ("Calling processKValues' 1 (-1) kStacks "++(show nextMinK)) $ processKValues' 1 (-1) kStacks nextMinK
--    where processKValues' i thisK ((k:kStack):kStacks) nextMinK = (newKStack:nextKStacks, thisK, nextNextMinK) -- throw our stack back onto the head of the list along with the computed results
--            where (nextKStacks, thisK, nextNextMinK) = processKValues' (i+1) newMinK kStacks nextMinK -- recurse because there are more stacks to check
--                  newMinK = if (thisK == -1 || k < thisK) then k else thisK -- compute the new min k
--                  newKStack = if (thisK == k) then kStack else (k:kStack) -- if we had the minimum value, then return our stack without that value, otherwise leave it untouched
--          processKValues' i thisK [] nextMinK
--            | (thisK >= nextMinK) = ([newKProgression], nextMinK, newNextMinK) -- if the computed next k value is >= the next min k, the next min k is now the current k and we generate the new k progression for the list
--            | otherwise = ([], thisK, nextMinK) -- otherwise, we proceed with all the values that were passed to us
--            where (_:newKProgression) = kProgression (i+1) -- ignore the head because it is nextMinK, which we are using right now
--                  newNextMinK = minK (i+2) -- compute the min k for the progression after this new one

processKValues' :: Integer -> Integer -> [[Integer]] -> Integer -> ([[Integer]], Integer, Integer)
processKValues' i trackingK ((k:kStack):kStacks) nextMinK = (newKStack:nextKStacks, finalK, nextNextMinK)
    where (nextKStacks, finalK, nextNextMinK) = trace ("Next k-value for stack i="++(show i)++" is "++(show k)++". New min-k is "++(show newMinK)++".") $ processKValues' (i+1) newMinK kStacks nextMinK
          newMinK = if (trackingK == -1 || k < trackingK) then (trace ("Our k-value "++(show k)++" is lower than the previous "++(show trackingK)) $ k) else (trace ("Our k-value "++(show k)++" is not lower than the previous "++(show trackingK)) $ trackingK)
          newKStack = if (finalK == k) then (trace ("Our k-value was equal to the final k-value, removing k from our stack") $ kStack) else (k:kStack)
processKValues' i thisK [] nextMinK
    | (thisK >= nextMinK) = trace ("Getting k-stack for new i="++(show (i+1))++". New min-k is "++(show nextMinK)++"New next min-k is "++(show newNextMinK)) $ ([newKProgression], nextMinK, newNextMinK)
    | otherwise = trace ("No new k-stack yet needed. min-k is "++(show thisK)++". Next min-k is still "++(show nextMinK)) $ ([], thisK, nextMinK)
    where (_:newKProgression) = kProgression (i+1)
          newNextMinK = minK (i+2)

-- the k progression for some i, that is, given an i generate all (i+j+2ij) where j is greater than or equal to i
kProgression :: Integer -> [Integer]
kProgression i = [2*i+2*i*i,2*i+1+2*i*i+2*i..]

-- the minimum k for some i, that is, given i generate (i+j+2ij) where j equals i
minK :: Integer -> Integer
minK i = (2*i+2*i*i)

-- Given a value n filtered from the list of integers using the sieve, compute a prime
computeOddPrime :: Integer -> Integer
computeOddPrime n = trace ("Emitting prime "++(show $ 2*n+1)++" calculated from non-k value "++(show n)) (2 * n + 1)

--------------------
-- Other prime APIs
--------------------

-- Prime factorization, generates in ascending order
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors num = factor:(primeFactors $ num `quot` factor)
    where factor = head $ filter (\x -> num `mod` x == 0) primeGenErt