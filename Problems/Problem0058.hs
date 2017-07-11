import Utils.Prime (isPrime)

main = print problem58Value

type I = Integer

problem58Value :: I
problem58Value = findSideLength 1 2 0 1

-- c: the 'counter' as we track the square spiral, a.k.a. the previous bottom right corner number
-- a: the number added to c to get each corner value of the next spiral
-- np: the number of prime diagonals tracked so far
-- nd: the number of total diagonals tracked so far
findSideLength :: I -> I -> I -> I -> I
findSideLength c a np nd
    | (np' * 100 `div` nd') < 10 = a+1 -- the side length is a+1
    | otherwise            = findSideLength (c+4*a) (a+2) np' nd' -- 
    where np' = np + (toInteger $ length [n | n <- [c+a,c+2*a..c+4*a], isPrime n])
          nd' = nd + 4
