import Data.List (foldl', dropWhile)
import qualified Data.Map as Map

main = print problem31Value

-- compute the number of ways to make change for 2 pounds (200 pence)
problem31Value :: Integer
problem31Value = waysToMakeChange 200

-- denominations of British currency
denominations :: [Integer]
denominations = [1,2,5,10,20,50,100,200]

-- get all denominations greater than or equal to a given value
getDenominations :: Integer -> [Integer]
getDenominations min = dropWhile (<min) denominations

-- call into the memoized version
waysToMakeChange :: Integer -> Integer
waysToMakeChange x = fst $ waysToMakeChangeMemo x 0 $ Map.empty

-- Map of (x, min) to (out) where x is the amount we are making change of, min is the minimum denomination to consider, and out is the computed WTMC value
-- The 'min' value is considered so that we maintain a particular sort order so that we can ignore duplicates.
type WtmcMap = (Map.Map (Integer, Integer) Integer)

-- Memoized function to compute the number of ways to make change (WTMC) for a given value.
-- Do a map lookup, if there is no entry, compute the value by subtracting every denomination
-- and computing the WTMC value of the resulting value.
-- We only consider denominations >= the previously subtracted value so the resulting combination of denominations is ordered.
waysToMakeChangeMemo :: Integer -> Integer -> WtmcMap -> (Integer, WtmcMap)
waysToMakeChangeMemo x min m = case (Map.lookup (x, min) m) of (Just value) -> (value, m)
                                                               Nothing -> (value', m')
    where (value', m') = if (x < 0) then (0, m)
                         else if (x == 0) then (1, m) 
                         else (sumSubValues, m'')
          (sumSubValues, m'') = foldl' (wtmcFold x) (0, m) (getDenominations min)

-- Fold function to compute the aggregated sum and memoization map for a given input value 'x', ('sum', 'map') aggregate, and 'i', the denomination to subtract.
wtmcFold :: Integer -> (Integer, WtmcMap) -> Integer -> (Integer, WtmcMap)
wtmcFold x (foldsum, foldm) i = let (out, outm) = (waysToMakeChangeMemo (x-i) i foldm) in (foldsum + out, Map.insert (x-i, i) out outm)
