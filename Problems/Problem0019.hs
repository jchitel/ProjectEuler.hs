main = print getProblem19Value

getProblem19Value :: Int
getProblem19Value = sum $ map computeNumSundaysOfYear [1901..2000]

computeNumSundaysOfYear :: Int -> Int
computeNumSundaysOfYear year
    | (year `mod` 4 == 0) = getNumSundaysForLeapYear $ computeStartingDowOfYear year
    | otherwise           = getNumSundaysForNonLeapYear $ computeStartingDowOfYear year

-- 1900 had 365 days, and 365 mod 7 is 1
-- if 1900 started with a monday, 1901 started with a tuesday (offset 2)
-- every year from there adds 1 to the DOW offset for that year
-- but we need to include leap years, which add 1 to the offset every 4 years
-- so we can compute the DOW offset for a given year using (2 + (x - 1901) + (x - 1901)/4) % 7
-- or (2 + ((x - 1901) * 5 / 4)) % 7
computeStartingDowOfYear :: Int -> Int
computeStartingDowOfYear year = (2 + ((year - 1901) * 5 `div` 4)) `mod` 7

-- LOGIC FOR THE FOLLOWING FUNCTIONS:
-------------------------------------
-- here is the sequence of month lengths for a non-leap year:
-- 31,28,31,30,31,30,31,31,30,31,30,31
-- here is the sequence of month lengths for a leap year:
-- 31,29,31,30,31,30,31,31,30,31,30,31
-- here are the same sequences, but using mod7:
-- 3,0,3,2,3,2,3,3,2,3,2,3
-- 3,1,3,2,3,2,3,3,2,3,2,3
-- and here is the sequence of offsets from the starting DOW of the year, for each month, using the above sequences:
-- 0,3,3,6,1,4,6,2,5,0,3,5
-- 0,3,4,0,2,5,0,3,6,1,4,6
-- given the index of the starting DOW of a year, which we can calculate, all months whose offsets add to 0 for that index start with a sunday.
-- non-leap sequences where the input is the starting DOW of the year, and the number of 0's (sundays) in that sequence:
-- 0:0,3,3,6,1,4,6,2,5,0,3,5 = 2
-- 1:1,4,4,0,2,5,0,3,6,1,4,6 = 2
-- 2:2,5,5,1,3,6,1,4,0,2,5,0 = 2
-- 3:3,6,6,2,4,0,2,5,1,3,6,1 = 1
-- 4:4,0,0,3,5,1,3,6,2,4,0,2 = 3
-- 5:5,1,1,4,6,2,4,0,3,5,1,3 = 1
-- 6:6,2,2,5,0,3,5,1,4,6,2,4 = 1
-- leap sequences where the input is the starting DOW of the year, and the number of 0's (sundays) in that sequence:
-- 0:0,3,4,0,2,5,0,3,6,1,4,6 = 3
-- 1:1,4,5,1,3,6,1,4,0,2,5,0 = 2
-- 2:2,5,6,2,4,0,2,5,1,3,6,1 = 1
-- 3:3,6,0,3,5,1,3,6,2,4,0,2 = 2
-- 4:4,0,1,4,6,2,4,0,3,5,1,3 = 2
-- 5:5,1,2,5,0,3,5,1,4,6,2,4 = 1
-- 6:6,2,3,6,1,4,6,2,5,0,3,5 = 1

getNumSundaysForNonLeapYear :: Int -> Int
getNumSundaysForNonLeapYear 0 = 2
getNumSundaysForNonLeapYear 1 = 2
getNumSundaysForNonLeapYear 2 = 2
getNumSundaysForNonLeapYear 3 = 1
getNumSundaysForNonLeapYear 4 = 3
getNumSundaysForNonLeapYear 5 = 1
getNumSundaysForNonLeapYear 6 = 1

getNumSundaysForLeapYear :: Int -> Int
getNumSundaysForLeapYear 0 = 3
getNumSundaysForLeapYear 1 = 2
getNumSundaysForLeapYear 2 = 1
getNumSundaysForLeapYear 3 = 2
getNumSundaysForLeapYear 4 = 2
getNumSundaysForLeapYear 5 = 1
getNumSundaysForLeapYear 6 = 1
