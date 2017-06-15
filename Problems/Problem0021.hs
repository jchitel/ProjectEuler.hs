import Utils.IntegerOps (properDivisors)

main = print problem21Value

problem21Value :: Integer
problem21Value = sum $ takeWhile (<10000) amicableNumbers

amicableNumbers :: [Integer]
amicableNumbers = filter isAmicable [1..]

isAmicable :: Integer -> Bool
isAmicable num = (sumProperSumProper == num) && (sumProper /= num)
    where sumProperSumProper = sum $ properDivisors sumProper
          sumProper = sum $ properDivisors num
