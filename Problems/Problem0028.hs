main = print problem28Value

problem28Value :: Integer
problem28Value = sumOfClockwiseGridDiagonals 1001

-- So this is fun.
-- I judged this to be 4*(sum of squares of [3,5..n]) - 6*(sum of (n-1) of [3,5..n]) + 1
-- 1st term: the top right corner of each "ring" is n^2, and the other 3 corners are relative to this one, so take the sum of all the odd squares from 3 to n and multiply it by 4.
--   The sum of squares from 1 to n is n(n+1)(2n+1)/6, and the sum of squares of 2n is 4*(that), so we take the sum of squares, subtract the sum of squares of evens (using (n-1)/2), then subtract 1, reduce,
--   and we're left with the sum of squares of odds from 3 to n: n(n+1)(n+2)/6 - 1.
--   Multiply that by 4: 2n(n+1)(n+2)/3 - 4
-- 2nd term: now we need to subtract for the other 3 corners. The first corner is n^2 - (n-1), the second is n^2 - 2(n-1), and the third is n^2 - 3(n-1).
--   So they are all together 4n^2 - 6(n-1), so we take the sum of (n-1) for all odds from 3 to n and multiply by 6 for the second term.
--   The sum of all numbers from 1 to n is n(n+1)/2, so we take (n-1) and divide by 2 so that we have the sum from 1 to (n-1)/2, then put this in for n, reduce, and multiply by 2 to get the sum of evens: (n+1)(n-1)/4.
--   Multiply that by 6: 3(n+1)(n-1)/2
-- 3rd term: The 1 at the center doesn't fit into the above formulas, so we just add it at the end: (+ 1)
-- Result: 2n(n+1)(n+2)/3 - 3(n+1)(n-1)/2 - 3
sumOfClockwiseGridDiagonals :: Integer -> Integer
sumOfClockwiseGridDiagonals n = (2*n*(n+1)*(n+2) `div` 3) - (3*(n+1)*(n-1) `div` 2) - 3
