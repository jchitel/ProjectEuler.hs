import Data.Char (digitToInt)

main = print problem56Value

problem56Value :: Int
problem56Value = maximum [sum $ map digitToInt $ show $ a ^ b | a <- [1..99], b <- [1..99]]
