{-
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
----
Ugh.
So we can identify digits to replace using a bit mask number, e.g. 1 will replace the last digit, 2 will replace the second-last digit, 3 will replace the last 2 digits, etc.
The max mask for a number of n digits is 2^n-1, e.g.
n=1: 2^1-1 = 1
n=2: 2^2-1 = 3
n=3: 2^3-1 = 7
n=4: 2^4-1 = 15

We need to convert a bit mask number to a bit mask array of bools. We can do this by alternating modding the number by 2 and dividing by 2, generating True when the mod is 1, and False when it is 0.
Ex: 18
18 % 2 = 0
9 % 2 = 1
4 % 2 = 0
2 % 2 = 0
1 % 2 = 1
This will generate a sequence from right to left. We can then zip this list of booleans with our number as a string. If we encounter a True, we replace the digit, otherwise we leave it the same.

So for each prime p (with string sp and digits dp), we iterate bitmasks b from 1 to 2^dp-1.
Then we convert b to a bit mask list bl.
Then we iterate replacements r from 0-9.
For each r, we zip bl over sp and then convert the result back to a number rp.
If rp has fewer than dp digits (i.e. we replaced the most significant digit with a 0), we ignore it.
We then count the resulting rps that are prime as crp.
If crp is higher than the previous max crp, replace the max values with these new ones.
Continue until max crp is 8.

So, just a prediction, this is likely to be very slow.
For each prime, we iterate from 1 to 2^dp-1, which is likely to be 63, hopefully not up to 127.
So if we say 63, we then multiply that by 10, so we are doing on average 630 operations on each prime.
If we start with 56003, we skip a lot of crap, so that will be nice.
Also, because we're dealing with digit replacements, we are going to end up with duplicate operations, so it may be valuable to memoize.
Meanwhile, I will try to come up with an alternative solution.
-}
main = print problem51Value

problem51Value :: Integer
problem51Value = 

getBitMask :: Int -> [Bool]
getBitMask n = (n `mod` 2 == 1)
