-- Get the lowest common multiple of all integers between 1 and 20, that is, the lowest number that is divisible by all numbers from 1 to 20

main = print getProblem5Value

getProblem5Value :: Integer
getProblem5Value = getLeastCommonMultiple [1..20]

-- Lowest Common Multiple: takes a list of numbers and returns the lowest common multiple of those numbers
getLeastCommonMultiple :: [Integer] -> Integer
getLeastCommonMultiple list = getLCM 1 [] list -- getLCM n factn is: 'n' is the product so far, 'factn' is the tracked prime factorization of 'n', 'is' is the remaining numbers to factor into the multiple
    where getLCM n _ [] = n
          getLCM n factn (i:is)
            | i' == 1 = getLCM n factn is -- if i' is 1, we can ignore it
            | otherwise = getLCM (n*i') (i':factn) is -- otherwise we multiply n by i', add it to the prime factors of n, and continue
            where i' = divByAll factn i -- we want the lowest multiple, so any numbers multiplied into n already should be factored out of i

-- this will factor each number in a list of numbers out of a target number, only if the target is divisible by each number
divByAll :: [Integer] -> Integer -> Integer
divByAll [] target = target
divByAll (i:is) target
    | target `mod` i == 0 = divByAll is (target `div` i)
    | otherwise = divByAll is target