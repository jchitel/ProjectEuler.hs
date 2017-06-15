import qualified Data.Map as Map

main = print getProblem18Value

-- the problem is to find the hightest sum of any path through the given grid
getProblem18Value :: Integer
getProblem18Value = fst $ gridLookupMemo (getGridWithIndices getGrid) Map.empty

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

-- Problem input data
getGrid :: [[Integer]]
getGrid = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23] ]