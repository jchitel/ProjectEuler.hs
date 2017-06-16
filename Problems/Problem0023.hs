import Utils.IntegerOps
import Utils.ListOps
import Data.Set (Set, toList, fromList, empty, insert, (\\))

main = print problem23Value

problem23Value :: Integer
problem23Value = sum $ toList resultSet

-- to compute the result set of numbers (every positive integer which is NOT a sum of 2 abundant numbers), we take all positive integers <28123 and subtract the numbers that ARE a sum of 2 abundant numbers
resultSet :: (Set Integer)
resultSet = (positiveIntegers 28123) \\ numbersThatAreSumOfTwoAbundantNumbers

positiveIntegers :: Integer -> (Set Integer)
positiveIntegers max = fromList [1..max]

-- we want to add together each pair of numbers from both lists, so long as the sum would be <= 28123
-- but we don't want duplicates, so that means that as we increment the left list, we don't care about items in the right list that are less than or equal to that number, because they've already been used
-- so that means that for every i of the left list, we want every j such that i<=j<=(28123-i), or dropWhile (<i) $ takeUntil (>28123-i) $ js
numbersThatAreSumOfTwoAbundantNumbers :: (Set Integer)
numbersThatAreSumOfTwoAbundantNumbers = addEmUp consideredNumbers consideredNumbers
    where addEmUp (i:[]) [] = empty
          addEmUp (i:i':is) [] = addEmUp (i':is) $ takeUntil (>(28123-i')) $ dropWhile (<i') consideredNumbers
          addEmUp (i:is) (j:js) = insert (i+j) (addEmUp (i:is) js)
          consideredNumbers = takeWhile (<28123) abundantNumbers

abundantNumbers :: [Integer]
abundantNumbers = abundantNumbers' [1..]
    where abundantNumbers' (i:rem)
            | sumDiv > i = i:(abundantNumbers' rem)
            | otherwise = abundantNumbers' rem
            where sumDiv = sum $ properDivisors i
