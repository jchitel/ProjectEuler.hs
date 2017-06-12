-- sum of all primes < 2 million

import Utils.Prime

main = print getProblem10Value

getProblem10Value :: Integer
getProblem10Value = sum $ takeWhile (<2000000) primeGen -- This is way too slow, even with Sundaram, it took >20 minutes
