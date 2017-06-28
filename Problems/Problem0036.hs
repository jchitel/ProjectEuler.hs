{-
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
-}
import Data.List (foldl')

main = print problem36Value

problem36Value :: Integer
problem36Value = sum $ filter isPalindromicInBase2 palindromicNumbers

palindromicNumbers :: [Integer]
palindromicNumbers = foldl' foldPalindrome [] [1..999]
    where foldPalindrome list x = p1:p2:list
              where sx = show x
                    p1 = read $ sx ++ reverse sx
                    p2 = read $ sx ++ (tail $ reverse sx)

isPalindromicInBase2 :: Integer -> Bool
isPalindromicInBase2 num = let str = (showBase2 num) in str == reverse str

showBase2 :: Integer -> String
showBase2 0 = []
showBase2 num = (showBase2 $ num `div` 2) ++ (show $ num `mod` 2)
