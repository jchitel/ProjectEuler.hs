
main = print getProblem17Value

-- Sets of numbers:
-- 1-19: fixed number of digits for each number (19)
-- [2-9]0: tens place from 2-9 (8)
-- [2-9][1-9]: tens place from 2-9 plus ones place from 1-9 (8*9=72)
-- [1-9]00: hundreds place from 1-9 plus 'hundred' (9)
-- [1-9][01-19]: hundreds place from 1-9 plus 'hundred and' plus 1-19 (9*19=171)
-- [1-9][2-9]0: hundreds place from 1-9 plus 'hundred and' plus tens place from 2-9 (9*8=72)
-- [1-9][2-9][1-9]: hundreds place from 1-9 plus 'hundred and' plus tens place from 2-9 plus ones place from 1-9 (9*8*9=648)
-- 1000: 'one thousand' = 11 digits (1)
getProblem17Value :: Integer
getProblem17Value = oneToNineteen + tens + tens_oneToNine + hundreds + hundreds_oneToNineteen + hundreds_tens + hundreds_tens_oneToNine + oneThousand
    where hundred = 7
          hundredAnd = 10
          oneThousand = 11
          oneToNine = sum $ map getNumDigits [1..9]
          oneToNineteen = sum $ map getNumDigits [1..19]
          tens = sum $ map getNumDigits [20,30..90]
          tens_oneToNine = 8 * oneToNine + 9 * tens
          hundreds = oneToNine + 9 * hundred
          hundreds_oneToNineteen = 19 * oneToNine + 9 * 19 * hundredAnd + 9 * oneToNineteen
          hundreds_tens = 8 * oneToNine + 9 * 8 * hundredAnd + 9 * tens
          hundreds_tens_oneToNine = 8 * 9 * oneToNine + 9 * 8 * 9 * hundredAnd + 9 * 9 * tens + 9 * 8 * oneToNine

getNumDigits :: Integer -> Integer
getNumDigits 1 = 3  -- one
getNumDigits 2 = 3  -- two
getNumDigits 3 = 5  -- three
getNumDigits 4 = 4  -- four
getNumDigits 5 = 4  -- five
getNumDigits 6 = 3  -- six
getNumDigits 7 = 5  -- seven
getNumDigits 8 = 5  -- eight
getNumDigits 9 = 4  -- nine
getNumDigits 10 = 3 -- ten
getNumDigits 11 = 6 -- eleven
getNumDigits 12 = 6 -- twelve
getNumDigits 13 = 8 -- thirteen
getNumDigits 14 = 8 -- fourteen
getNumDigits 15 = 7 -- fifteen
getNumDigits 16 = 7 -- sixteen
getNumDigits 17 = 9 -- seventeen
getNumDigits 18 = 8 -- eighteen
getNumDigits 19 = 8 -- nineteen
getNumDigits 20 = 6 -- twenty
getNumDigits 30 = 6 -- thirty
getNumDigits 40 = 5 -- forty
getNumDigits 50 = 5 -- fifty
getNumDigits 60 = 5 -- sixty
getNumDigits 70 = 7 -- seventy
getNumDigits 80 = 6 -- eighty
getNumDigits 90 = 6 -- ninety
