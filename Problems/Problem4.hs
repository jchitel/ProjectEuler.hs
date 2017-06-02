-- Largest palindrome made from the product of 2 3-digit numbers

main = print getProblem4Value

getProblem4Value :: Integer
getProblem4Value = findLargestPal 997 999

findLargestPal :: Integer -> Integer -> Integer
findLargestPal pal i
    | i < 100 = findLargestPal (pal-1) 999
    | (palindrome `mod` i /= 0) = findLargestPal pal (i-1)
    | (palindrome `mod` i == 0) && (palindrome `div` i >= 100) && (palindrome `div` i < 1000) = palindrome
    | otherwise = findLargestPal (pal-1) 999
    where palindrome = (read ((show pal) ++ reverse (show pal)))
