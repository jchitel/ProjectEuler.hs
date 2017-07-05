import Utils.Prime (isPrime, primeGen)

main = print problem46Value

-- the first positive odd composite integer that is rejected by the conjecture
problem46Value :: Integer
problem46Value = head [n | n <- [3..], n `mod` 2 == 1, not $ isPrime n, rejected n]

-- a number is rejected if there are no primes p <n where (n-p)/2 is a perfect square
rejected :: Integer -> Bool
rejected n = length [1 | p <- takeWhile (<n) primeGen, (n-p) `mod` 2 == 0, isPerfectSquare ((n-p) `div` 2)] == 0

-- an integer n is a perfect square if the square of the floor of its square root is equal to n
isPerfectSquare :: Integer -> Bool
isPerfectSquare n = (root ^ 2) == n
    where root = floor $ sqrt $ fromIntegral n
