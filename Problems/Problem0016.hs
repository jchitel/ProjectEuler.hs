
main = print getProblem16Value

getProblem16Value :: Integer
getProblem16Value = sum $ map (read . (\x -> [x])) $ show (2 ^ 1000)

