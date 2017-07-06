import Utils.Prime (isPrime)
import Data.List (sort, permutations)

main = print problem49Value

problem49Value :: Integer
problem49Value =  combinify (getSequence !! 2)

combinify :: (Int, Int, Int) -> Integer
combinify (x,y,z) = read $ concat [show x, show y, show z]

getSequence :: [(Int, Int, Int)]
getSequence = filter qualifier [(x,p1,p2) | x <- [1111..9999], p1 <- permutationInts x, p2 <- permutationInts x, x < p1, p1 < p2]

permutationInts :: Int -> [Int]
permutationInts x = map read $ permutations $ show x

qualifier :: (Int, Int, Int) -> Bool
qualifier (x,y,z) = (isPrime x) && (isPrime y) && (isPrime z) && (z-y == y-x)
