import Data.List (sort)

main = print problem52Value

problem52Value :: Integer
problem52Value = head $ [x | x <- [1..], all (isPermutation x) [2*x,3*x..6*x]]

isPermutation :: Integer -> Integer -> Bool
isPermutation a b = (sort $ show a) == (sort $ show b)
