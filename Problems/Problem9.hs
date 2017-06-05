-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- (1000 - b - c)^2 + b^2 = c^2
-- (1000 - b - c)(1000 - b - c) + b^2 = c^2
-- 1e6 + b^2 + c^2 - 2000b - 2000c + 2bc + b^2 = c^2
-- 2b^2 - 2000b - 2000c + 2bc + 1e6 = 0
-- 2b^2 + (2c - 2000)b - 2000c + 1e6 = 0
-- a = 2, b = 2c - 2000, c = 1e6 - 2000c
-- b = ((2000 - 2c) + sqrt((2c - 2000)^2 - 8(1e6 - 2000c)))/4 OR ((2000 - 2c) - sqrt((2c - 2000)^2 - 8(1e6 - 2000c)))/4
-- b = ((2000 - 2c) +/- sqrt(4c^2 - 8000c + 4e6 - 8e6 - 16000c))/4
-- b = ((2000 - 2c) +/- sqrt(4c^2 - 24000c - 4e6))/4
-- b = ((2000 - 2c) +/- 2sqrt(c^2 - 6000c - 1e6))/4

main = print getProblem9Value

getProblem9Value :: Integer
getProblem9Value = (a*b*c)
    where (a, b, c) = findPythagoreanTripletFromSum 1000

-- Start with a=1, b=2, and c=sum-(a+b). Hold a constant, move b and c closer together until b is no longer less than c. Then increment a, set b=a+1 and c=sum-(a+b) and start over. Continue until a^2+b^2=c^2.
findPythagoreanTripletFromSum :: Integer -> (Integer, Integer, Integer)
findPythagoreanTripletFromSum sum = find' 1 2 (sum-3)
    where find' a b c
           | ((a^2 + b^2) == c^2) = (a, b, c) -- if they satisfy the pythagorean theorem, we have a match
           | ((b + 1) >= (c - 1) && (a + 1) >= b) = (-1, 0, 0) -- if incrementing b and decrementing c will result in !(b < c) and a can't increase anymore, fail (this shouldn't happen)
           | ((b + 1) >= (c - 1)) = find' (a + 1) (a + 2) (sum - (2 * a) - 3) -- if incrementing b and decrementing c will result in !(b < c), increment a, set b = a+1, and c = sum-2a-3
           | otherwise = find' a (b + 1) (c - 1) -- otherwise, move b and c closer together