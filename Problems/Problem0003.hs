-- 600851475143 

import Utils.Prime

main = putStrLn $ show getProblem3Value

getProblem3Value :: Integer
getProblem3Value = last $ primeFactors 600851475143
