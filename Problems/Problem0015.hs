
main = print getProblem15Value

getProblem15Value :: Integer
getProblem15Value = getNumberOfPaths 20 20

getNumberOfPaths :: Integer -> Integer -> Integer
getNumberOfPaths x y = (fact (x+y)) `div` ((fact x) * (fact y))

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact x = x * (fact (x-1))
