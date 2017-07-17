import Utils.Memo (memoTree)

main = print problem92Value

-- run the cycle for every number from 1 to 1e8-1, get the number that end up at 89
problem92Value :: Int
problem92Value = length $ filter (==89) $ map digCycleMemo [1..9999999]

-- memoize the computation
digCycleMemo :: Integer -> Integer
digCycleMemo = memoTree digCycle

-- recursively compute the chain values
digCycle :: (Integer -> Integer) -> Integer -> Integer
digCycle _ 1 = 1
digCycle _ 89 = 89
digCycle mem x = mem $ sum $ map ((^2) . read . (:[])) $ show x
