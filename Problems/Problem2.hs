import Utils.Fibonacci

main = putStrLn $ show getProblem2Value

getProblem2Value :: Integer
getProblem2Value = sum $ filter even $ takeWhile (<4000000) fibSequence
