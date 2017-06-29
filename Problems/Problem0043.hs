import Data.List (permutations)

main = print problem43Value

problem43Value :: Integer
problem43Value = sum $ map read $ filter isQualified pandigitals

pandigitals :: [String]
pandigitals = permutations "0123456789"

readInteger :: String -> Integer
readInteger = read

isQualified :: String -> Bool
isQualified (d1:d2:d3:d4:d5:d6:d7:d8:d9:d10:[])
    | (readInteger [d2,d3,d4]) `mod` 2 /= 0   = False
    | (readInteger [d3,d4,d5]) `mod` 3 /= 0   = False
    | (readInteger [d4,d5,d6]) `mod` 5 /= 0   = False
    | (readInteger [d5,d6,d7]) `mod` 7 /= 0   = False
    | (readInteger [d6,d7,d8]) `mod` 11 /= 0  = False
    | (readInteger [d7,d8,d9]) `mod` 13 /= 0  = False
    | (readInteger [d8,d9,d10]) `mod` 17 /= 0 = False
    | otherwise                               = True
