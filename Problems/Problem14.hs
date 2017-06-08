
import Utils.IntegerOps

main = print getProblem14Value

getProblem14Value :: Integer
getProblem14Value = fst $ foldl (\(max, maxColl) next -> let nextColl = (collatzLength next) in if nextColl > maxColl then (next, nextColl) else (max, maxColl)) (1, 0) [1..1000000]

collatzLength :: Integer -> Int
collatzLength num = length $ collatz num
