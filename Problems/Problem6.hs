-- Get the difference between the sum of squares and the square of sum for the numbers between 1 and 100

main = print getProblem6Value

getProblem6Value :: Integer
getProblem6Value = getSquareOfSumMinusSumOfSquares [1..100]

getSquareOfSumMinusSumOfSquares :: [Integer] -> Integer
getSquareOfSumMinusSumOfSquares nums = (getSquareOfSum nums) - (getSumOfSquares nums)

getSquareOfSum :: [Integer] -> Integer
getSquareOfSum nums = (sum nums) ^ 2

getSumOfSquares :: [Integer] -> Integer
getSumOfSquares nums = sum $ map (^2) nums
