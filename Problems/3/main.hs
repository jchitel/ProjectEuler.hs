-- 600851475143 

import Utils.Prime

main = putStrLn $ show getProblem3Value

getProblem3Value :: Integer
getProblem3Value = maximum $ primeGenErt $ floor $ sqrt $ fromIntegral 600851475143
