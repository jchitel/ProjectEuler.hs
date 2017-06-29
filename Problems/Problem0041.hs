import Utils.Prime (isPrime)
import Data.List (permutations)

main = print problem41Value

problem41Value :: Integer
problem41Value = maximum $ filter isPrime pandigitals

pandigitals :: [Integer]
pandigitals = map read $ concat [permutations $ concat $ map show [1..n] | n <- [1..9]]
