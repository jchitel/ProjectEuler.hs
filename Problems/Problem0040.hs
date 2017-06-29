import Data.Char (digitToInt)

main = print problem40Value

problem40Value :: Int
problem40Value = d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
    where d1 = digitToInt $ list !! 0
          d10 = digitToInt $ list !! 9
          d100 = digitToInt $ list !! 99
          d1000 = digitToInt $ list !! 999
          d10000 = digitToInt $ list !! 9999
          d100000 = digitToInt $ list !! 99999
          d1000000 = digitToInt $ list !! 999999
          list = concat $ map show [1..]
