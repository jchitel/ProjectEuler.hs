module Utils.ListOps (takeUntil, dropUntil, filterMap) where
import Data.List (foldl')

-- takes all values of a list until (non-inclusive) a value returns true
-- so the result is the list where the last item is the last item in the list that returns false for the function
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil fn [] = []
takeUntil fn (i:is)
    | fn i = []
    | otherwise = i:(takeUntil fn is)

-- drops all values of a list until (non-inclusive) a value returns true
-- so the result is the list where the first item is the first item in the list that returns true for the function
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil fn [] = []
dropUntil fn (i:is)
    | fn i = i:is
    | otherwise = dropUntil fn is

-- performs both a map and a filter using a function that returns maybes
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap func list = map (\(Just x) -> x) $ filter (\x -> case x of { Just _ -> True; Nothing -> False }) $ map func list
