import Data.Char (chr, ord)
import Data.Bits (xor)
import Data.List (isInfixOf)

main = do
    text <- readFile "Problem0059.txt"
    print $ problem59Value text

-- extract the words from the text, convert them to Int, brute force all possible keys, look for the sequence " the ",
-- get the first one (should be the only one), convert the Chars to Ints, sum them
problem59Value :: String -> Int
problem59Value str = sum $ map ord $ head $ filter (isInfixOf " the ") [map chr $ decrypt bytes [a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122]]
    where bytes = map read $ words str

-- repeat the numbers in the key for the length of the ciphertext, then zip them together with xor
decrypt :: [Int] -> [Int] -> [Int]
decrypt ciphertext key = zipWith xor ciphertext keyrpt
    where keyrpt = take (length ciphertext) $ concat $ repeat key
