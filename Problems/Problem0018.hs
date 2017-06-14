import Data.List

main = print getProblem18Value

-- the problem is to find the hightest sum of any path through the given grid
getProblem18Value :: Integer
getProblem18Value = findHighestSum getRows

-- given a pyramidal grid of numbers, find the highest sum of any path
findHighestSum :: [[Integer]] -> Integer
findHighestSum grid = sum $ map (\(a,_,_) -> a) $ getMaximumPath grid

-- given a pyramidal grid of numbers, find the path with the highest sum
getMaximumPath :: [[Integer]] -> [(Integer, Int, Int)]
getMaximumPath grid = getMaximumPath' $ sortGridWithIndices $ getGridWithIndices grid -- convert the grid into a list of grid cells, tagged with the index of each cell, and sort that list
    where getMaximumPath' [] = [] --  base case
          getMaximumPath' (maxCell:remainingCells) = maxCell:(getMaximumPath' $ gridClosure maxCell remainingCells) -- take the head of the list (max value) and recurse with the closure of the remaining grid with that cell as the pivot

-- given a "pivot" cell and the remaining cells left in the grid, find the "closure" of the grid around that cell, which are all remaining cells that are reachable from that pivot
gridClosure :: (Integer, Int, Int) -> [(Integer, Int, Int)] -> [(Integer, Int, Int)]
gridClosure _ [] = [] -- base case
gridClosure pivot@(_,pivotI,pivotJ) (cell@(_,cellI,cellJ):gridWIndices) -- grab the pivot cell and the current cell, and the i and j indices of both
    | (cellI < pivotI && cellJ >= (pivotJ-pivotI+cellI) && cellJ <= pivotJ) = cell:(gridClosure pivot gridWIndices) -- if the current cell's row is < the pivot's row, its column must be between (pivotj-pivoti+celli) and pivotj
    | (cellI > pivotI && cellJ >= pivotJ && cellJ <= (pivotJ-pivotI+cellI)) = cell:(gridClosure pivot gridWIndices) -- if the current cell's row is > the pivot's row, its column must be between pivotj and (pivotj-pivoti+celli)
    | otherwise = gridClosure pivot gridWIndices -- if it doesn't match the above cases, we ignore the cell

-- given a list of grid cells with indices, sort it in descending order by cell value
sortGridWithIndices :: [(Integer, Int, Int)] -> [(Integer, Int, Int)]
sortGridWithIndices grid = reverse $ sortOn (\(a,_,_) -> a) grid

-- given a pyramidal grid of numbers, convert it into a flat list of tuples with the structure of (value, row, column)
getGridWithIndices :: [[Integer]] -> [(Integer, Int, Int)]
getGridWithIndices grid = getGridWithIndices' 0 grid
    where getGridWithIndices' _ [] = []
          getGridWithIndices' i (row:remGrid) = (getGridWithIndices'' 0 i row) ++ (getGridWithIndices' (i+1) remGrid)
          getGridWithIndices'' _ _ [] = []
          getGridWithIndices'' j i (cell:remRow) = (cell,i,j):(getGridWithIndices'' (j+1) i remRow)

-- Problem input data
getRows :: [[Integer]]
getRows = [
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