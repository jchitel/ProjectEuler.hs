-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

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