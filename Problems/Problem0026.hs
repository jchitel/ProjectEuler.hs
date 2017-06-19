import Data.List (elemIndex, foldl')

main = print problem26Value

problem26Value :: Integer
problem26Value = snd $ foldl' (\(maxLen, maxNum) next -> let maxNext = (lengthOfRecurringCycle next) in if (maxNext > maxLen) then (maxNext, next) else (maxLen, maxNum)) (0, 0) [1..1000]

lengthOfRecurringCycle :: Integer -> Int
lengthOfRecurringCycle num = lengthOfRecurringCycle' num 1 []

-- repeatedly multiply the fraction by 10 so that we can iteratively observe the sequence of digits, and track the remainder of the division each time
-- if we ever reach a remainder of 0, then it is not an infinite decimal, so it is disqualified from the algorithm
-- if we ever reach a remainder we've seen before, then however many digits since the last time determines the length of the sequence
lengthOfRecurringCycle' :: Integer -> Integer -> [Integer] -> Int
lengthOfRecurringCycle' denom numer modSeq
    | newMod == 0 = (-1)                                                                                                       -- if we reach a remainder of 0, then there is no recurring cycle
    | otherwise = case (elemIndex newMod modSeq) of (Just index) -> index + 1                                                -- we have seen the remainder before, so now we know the length of the recurring sequence
                                                    Nothing -> lengthOfRecurringCycle' denom (numer * 10) (newMod:modSeq)    -- the sequence (if any) continues, so we increment the tens place of the numerator and add the new mod
    where newMod = numer `mod` denom
