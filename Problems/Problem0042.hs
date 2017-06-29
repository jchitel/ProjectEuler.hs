import Data.Char (ord)

main = do
    str <- readFile "Problem0042.txt"
    print $ problem42Value str

problem42Value :: String -> Int
problem42Value str = length $ filter isTriangleWord $ lines str

isTriangleWord :: String -> Bool
isTriangleWord word = let wordValue = (sum $ map charValue word) in wordValue `elem` (takeWhile (<=wordValue) triangles)

charValue :: Char -> Int
charValue c = (ord c) - (ord 'A') + 1

triangles :: [Int]
triangles = takeWhile (< maxBound) [n*(n+1) `div` 2 | n <- [1..]]
