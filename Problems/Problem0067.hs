import qualified Data.Map as Map

main = do
    str <- readFile "Problem0067.txt"
    print $ getProblem67Value str

-- the problem is to find the hightest sum of any path through the given grid
getProblem67Value :: String -> Integer
getProblem67Value str = fst $ gridLookupMemo (getGridWithIndices grid) Map.empty
    where grid = map ((map read) . words) $ lines str

-- given a grid, strip off all the left-most values
stripLeftEdge :: [(Integer, Int, Int)] -> [(Integer, Int, Int)]
stripLeftEdge grid@((_,_,j0):cs) = filter (\(_,_,j) -> j /= j0) grid -- get the column of the first item (top of the pyramid) and filter out all cells in that column

-- given a grid, strip off all the right-most values
stripRightEdge :: [(Integer, Int, Int)] -> [(Integer, Int, Int)]
stripRightEdge grid@((_,_,j):gs) = stripRightEdge' j grid -- get the column of the first item (top of the pyramid), which is the current right-most value of the top row
    where stripRightEdge' _ [] = []
          stripRightEdge' j (cell@(_,_,j'):grid) -- get the column of the current item
           | j' == j = stripRightEdge' (j+1) grid -- if it matches the passed in column, drop it, increment the column, and recurse
           | otherwise = cell:(stripRightEdge' j grid) -- if not, include it in the result list and recurse without incrementing the column, as we haven't found the right-most value yet

-- given a grid and a map for memoization, compute the maximum sum of any path from the top to the bottom of the grid
gridLookupMemo :: [(Integer, Int, Int)] -> (Map.Map (Int,Int) Integer) -> (Integer, (Map.Map (Int,Int) Integer))
gridLookupMemo ((val,i,j):[]) map = (val, Map.insert (i,j) val map) -- we've shaved off the whole grid, so just save the value of this grid location
gridLookupMemo grid@((val,i,j):gs) map = case (Map.lookup (i,j) map) of (Just sum) -> (sum, map) -- map lookup, if there is a result, return it with the existing map
                                                                        Nothing -> (sum', map') -- otherwise compute the value and the new map
    where (sumRight, mapRight) = gridLookupMemo (stripLeftEdge grid) map -- compute the right-side max
          (sumLeft, mapLeft) = gridLookupMemo (stripRightEdge grid) mapRight -- compute the left-side max (pass the map from the right side result)
          sum' = val + (max sumLeft sumRight) -- the sum is the max of the two sides
          map' = Map.insert (i,j) sum' mapLeft -- add the new computed value to the map (pass the map from the left side result)

-- given a pyramidal grid of numbers, convert it into a flat list of tuples with the structure of (value, row, column)
getGridWithIndices :: [[Integer]] -> [(Integer, Int, Int)]
getGridWithIndices grid = getGridWithIndices' 0 grid
    where getGridWithIndices' _ [] = []
          getGridWithIndices' i (row:remGrid) = (getGridWithIndices'' 0 i row) ++ (getGridWithIndices' (i+1) remGrid)
          getGridWithIndices'' _ _ [] = []
          getGridWithIndices'' j i (cell:remRow) = (cell,i,j):(getGridWithIndices'' (j+1) i remRow)
