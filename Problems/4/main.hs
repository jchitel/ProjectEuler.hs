-- Largest palindrome made from the product of 2 3-digit numbers
-- A number n is palindromic if log10(n) is odd and for every i from 0 to log10(n) `div` 2, digitAt i num = digitAt (log10(n)-i) num

import Utils.IntegerOps

main = putStrLn $ show getProblem4Value

getProblem4Value :: Integer
getProblem4Value = intLogBase 10 9999
