{-
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

     1/56 1/42 1/30 1/20 1/28 1/21 1/24 1/40 1/35 1/14
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
                    2/8       2/6                 4/8                 4/6       6/8
                                                  2/4

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.
----
3/7 = 0.42857142857142855
The largest multiple of 7 under 1 million is 7*floor(1000000/7) = 999999, and 999999/7 = 142857.
So 3/7 = 428571/999999, they are the same number.
We can count from d = 3 to 1000000, finding the n which brings the fraction closest to 3/7 without going over.
d = 3: (3*3/7)/3 = (9/7)/3 -> 1/3
d = 4: (3*4/7)/4 = (12/7)/4 -> 1/4
d = 5: (15/7)/5 -> 2/5
d = 6: (18/7)/6 -> 2/6
d = 8: (24/7)/8 -> 3/8

import Utils.ListOps (minimumUsing)
import Utils.IntegerOps (divide)

main = print problem71Value

problem71Value :: Int
problem71Value = (`quot` 7) $ (*3) $ minimumUsing (\x -> ((x*3) `divide` 7) - (fromIntegral (x*3 `quot` 7))) $ filter (\x -> x `rem` 7 /= 0) [5..1000000]
