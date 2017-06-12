
main = putStrLn $ show getProblem1Value

getProblem1Value :: Integer
getProblem1Value = getSumOfMultiples [3, 5] 999

getSumOfMultiples :: [Integer] -> Integer -> Integer
getSumOfMultiples list 0 = 0
getSumOfMultiples list num = (if isMultiple list num then num else 0) + getSumOfMultiples list (num - 1)

isMultiple :: [Integer] -> Integer -> Bool
isMultiple [] _ = False
isMultiple (x:xs) num
    | num `mod` x == 0 = True
    | otherwise = isMultiple xs num
