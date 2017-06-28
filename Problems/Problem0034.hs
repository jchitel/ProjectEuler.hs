import Data.Char (digitToInt)

main = print problem34Value

problem34Value :: Int
problem34Value = sum $ filter isQualified [10..9999999]

isQualified :: Int -> Bool
isQualified num = num == (sum $ map fact $ digits num)

digits :: Int -> [Int]
digits = (map digitToInt) . show

fact :: Int -> Int
fact n = product [1..n]
