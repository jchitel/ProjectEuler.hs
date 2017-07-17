main = print problem63Value

problem63Value :: Int
problem63Value = getCount 1

getCount :: Int -> Int
getCount p
    | matches == 0 = 0
    | otherwise = matches + (getCount (p+1))
    where matches = length $ filter (\x -> (length $ show x) == p) $ map (^p) [1..9]
