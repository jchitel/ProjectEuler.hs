main = print getProblem30Value

getProblem30Value :: Integer
getProblem30Value = sum $ filter isEqualToSumOfFifthPowersOfDigits [10..999999]

isEqualToSumOfFifthPowersOfDigits :: Integer -> Bool
isEqualToSumOfFifthPowersOfDigits num = num == (sum $ map ((^5) . read . (:[])) $ show num)
