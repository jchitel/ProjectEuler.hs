import Utils.Prime (isPrime)
import Data.List (foldr)

main = print problem51Value

problem51Value :: Int
problem51Value = find8PrimeValueFamily primes (bitmasks $ head primes) [0..9] 0

-- start with 56003 because that's the number that is given in the problem definition as the first 7-prime family,
-- so the first 8-prime family must necessarily be higher
primes :: [Int]
primes = filter isPrime [56003..]

intToDigit :: Int -> Char
intToDigit = head . show

-- all the possible bitmasks of a number of 'dn' digits are 1 to (2^dn-1)
bitmasks :: Int -> [Int]
bitmasks n = let dn = (length $ show n) in [1..(2^dn-1)]

-- Given a list of primes, a list of bitmasks for that prime, a list of numbers to use for replacements in the prime, and a count of primes for the current prime-bitmask combination,
-- find the prime of the first prime-bitmask combination to produce 8 primes when the digits of the prime are replaced using the bitmask.
find8PrimeValueFamily :: [Int] -> [Int] -> [Int] -> Int -> Int
find8PrimeValueFamily (p:p1:ps) []     _  _  = find8PrimeValueFamily (p1:ps) (bitmasks p1) [0..9] 0 -- once we're out of bitmasks, move to the next prime
find8PrimeValueFamily ps        (b:bs) [] _  = find8PrimeValueFamily ps      bs            [0..9] 0 -- once we're out of replacement digits, move to the next bitmask
find8PrimeValueFamily (p:ps) (b:bs) (r:rs) c
    | drp < dp                 = find8PrimeValueFamily (p:ps) (b:bs) rs c     -- the result of the replacements had too few digits, skip it
    | (c == 7) && (isPrime rp) = getMinimumOfFamily sp b                      -- we already had 7 primes in this family and this one is prime, so we've found 8, get the minimum of the family as the SOLUTION
    | isPrime rp               = find8PrimeValueFamily (p:ps) (b:bs) rs (c+1) -- the number is prime but we had less than 7 already, just increment the counter
    | otherwise                = find8PrimeValueFamily (p:ps) (b:bs) rs c     -- otherwise, the number is not prime and we can't count it
    where sp = show p                                    -- sp = the prime as a string
          dp = length sp                                 -- dp = the number of digits in p
          rp = read $ replaceBitmask sp b (intToDigit r) -- rp = the replaced number
          drp = length $ show rp                         -- drp = the number of digits in rp (we need to convert to Int and back to String to remove 0's at the start)

-- using 'b' as a bitmask, replace each digit in 's' corresponding to a bit in 'b' with the character 'r'
-- bits can be extracted from the bitmask using iterative division by 2, which generates a sequence from LSB to MSB, so we need to use foldr on the string
replaceBitmask :: String -> Int -> Char -> String
replaceBitmask s b r = fst $ foldr (\c (rs, b') -> let (q, rm) = (b' `quotRem` 2) in if (rm == 0) then (c:rs, q) else (r:rs, q)) ([], b) s

-- once we have a family (prime-bitmask combination), this gets the smallest member of the family
getMinimumOfFamily :: String -> Int -> Int
getMinimumOfFamily s b = minimum $ filter isPrime $ map read $ filter (\x -> (length x) == (length s)) $ map (replaceBitmask s b) $ map intToDigit [0..9]
