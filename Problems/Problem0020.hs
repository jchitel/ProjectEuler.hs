main = print problem20Value

problem20Value :: Int
problem20Value = sum $ map (\x -> read [x]) $ show $ giantFact
    where giantFact = product [1..100] :: Integer
