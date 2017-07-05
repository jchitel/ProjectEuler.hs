main = print problem45Value

problem45Value :: Integer
problem45Value = head [n | n <- hexagons, isPent n]

hexagons :: [Integer]
hexagons = map (\n -> n * (2*n - 1)) [144..]

-- using the inverse pentagonal formula (1+sqrt(1+24p))/6, determine if the result would be an integer
isPent :: Integer -> Bool
isPent p = p > 0 && (root ^ 2 == square) && ((root + 1) `mod` 6 == 0)
    where square = (p * 24) + 1
          root = floor $ sqrt $ fromIntegral square
