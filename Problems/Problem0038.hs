import Data.List (sort)

main = print problem38Value

problem38Value :: Integer
problem38Value = maximum $ filter isPandigital $ generateNumbers 1

generateNumbers :: Integer -> [Integer]
generateNumbers m
    | out == [] = []
    | otherwise = out ++ (generateNumbers (m+1))
    where out = generateNumbers' m 2

generateNumbers' :: Integer -> Integer -> [Integer]
generateNumbers' m n
    | out >= 1000000000 = []
    | otherwise = out:(generateNumbers' m (n+1))
    where out = read $ concat $ map show [m*i | i <- [1..n]]

isPandigital :: Integer -> Bool
isPandigital n = (sort $ show n) == "123456789"
