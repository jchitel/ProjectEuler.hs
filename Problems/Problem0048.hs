main = print problem48Value

problem48Value :: Integer
problem48Value = read $ reverse $ take 10 $ reverse $ show $ sum $ map (\x -> x ^ x) [1..1000]
