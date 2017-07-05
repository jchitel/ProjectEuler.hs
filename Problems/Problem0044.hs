main = print problem44Value

-- starting with s (sum, j+k) at the third pentagon and j at the highest pentagon <= s/2, find the first d (s-2j) where s-j (k) and d are pentagons
problem44Value :: Integer
problem44Value = head [d | s <- drop 2 pentagons, j <- reverse $ takeWhile (<=(s `div` 2)) pentagons, d <- [s-2*j], isPent (s-j), isPent (s-2*j)]

-- list of all pentagons starting with 1
pentagons :: [Integer]
pentagons = map pent [1..]

-- pentagonal formula n(3n-1)/2
pent :: Integer -> Integer
pent n = n*(3*n - 1) `div` 2

-- using the inverse pentagonal formula (1+sqrt(1+24p))/6, determine if the result would be an integer
isPent :: Integer -> Bool
isPent p = p > 0 && (root ^ 2 == square) && ((root + 1) `mod` 6 == 0)
    where square = (p * 24) + 1
          root = floor $ sqrt $ fromIntegral square
