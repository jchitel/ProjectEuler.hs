
import qualified Data.Map as Map

main = print getProblem15Value

getProblem15Value :: Integer
getProblem15Value = getNumberOfPaths 20 20

getNumberOfPaths :: Integer -> Integer -> Integer
getNumberOfPaths x y = fst $ getNumberOfPaths' Map.Empty x y
    where getNumberOfPaths' map x y = case (Map.lookup (x,y) map) of (Just num) -> (num, map)   -- lookup the pair in the map. if it's there, return that value and the existing map
                                                                     Nothing -> (sum, newMap'') -- if it's not there, return the computed value and map
           where sum                = numDown + numRight                -- the computed value is the sum of the number of paths going either down or right
                 newMap''           = Map.insert (x,y) sum newMap'      -- the new computed map to use is the map created by inserting the result of this pair into the resulting map from the down and right lookups
                 (numDown, newMap') = getNumberOfPaths' newMap x (y-1)  -- looking up the down value requires recursing with y-1 and the resulting map from the right lookup
                 (numRight, newMap) = getNumberOfPaths' map (x-1) y     -- looking up the right value requires recursing with x-1 and the map passed into the function

getNumberOfPaths x y = mem_getNumberOfPaths' $ (x,y)
    where mem_getNumberOfPaths' = memoize getNumberOfPaths'
          getNumberOfPaths' (x,y) = (mem_getNumberOfPaths' (x-1) y) + (mem_getNumberOfPaths' x (y-1))
          getNumberOfPaths' (0,_) = 1
          getNumberOfPaths' (_,0) = 1

memoize :: (a -> b) -> (a -> b)
memoize func x = fst $ memoize' Map.Empty func x
    where memoize' map func' x' = case (Map.lookup x' map) of (Just y) -> (y, map)
                                                              Nothing -> (y', newMap'')
           where y' = func' x'
                 map' = Map.insert x' y' map






(a -> b) memoize((a -> b) func) => {
    
}