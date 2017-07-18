import Utils.Prime (primes)

main = print problem69Value

problem69Value :: Int
problem69Value = last $ takeWhile (<1000000) [product $ take n primes | n <- [1..]]
