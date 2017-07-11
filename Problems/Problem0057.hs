main = print problem57Value

problem57Value :: Int
problem57Value = length $ filter (\(n, d) -> (length $ show n) > (length $ show d)) $ take 1000 root2Expansions

root2Expansions :: [(Integer, Integer)]
root2Expansions = (3, 2):(expand (3, 2))
    where expand (n, d) = let f = (n+2*d, n+d) in f:(expand f)
