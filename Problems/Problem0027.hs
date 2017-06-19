import Utils.Prime (primeGen, isPrime)
import Data.List (foldl')
import Debug.Trace

main = print problem27Value

problem27Value :: Integer
problem27Value = let (a,b) = (snd $ foldl' maxPrimes (0,(0,0)) abs) in a*b
    where maxPrimes (maxVal, (maxA,maxB)) (a,b) = let nextVal = numPrimes a b in if nextVal > maxVal then (nextVal, (a,b)) else (maxVal, (maxA,maxB))
          abs = [(a,b) | a <- [-999..999], b <- takeWhile (<1000) primeGen]

numPrimes :: Integer -> Integer -> Int
numPrimes a b = length $ takeWhile (isPrime) $ map (\x -> x^2 + a*x + b) [0..]
