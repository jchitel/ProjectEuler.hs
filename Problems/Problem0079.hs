import Data.List (foldl', elemIndex)

main = do
    text <- readFile "Problem0079.txt"
    print $ problem79Value text

problem79Value :: String -> Integer
problem79Value str = read $ sortCodes $ lines str

sortCodes :: [String] -> String
sortCodes [] = []
sortCodes codes = hd:(sortCodes filtered)
    where filtered = filter (\x -> (length x) /= 0) removed                 -- filter any codes that have been exhausted
          hd = foldl' findFirst (head $ head codes) codes                   -- find the first character in sort order in the whole list
          removed = map (\(x:xs) -> if (x == hd) then xs else (x:xs)) codes -- remove that character from each code

-- fold function that finds the only character that is always a first occurrence in each of a list of strings
findFirst :: Char -> String -> Char
findFirst f code = case (elemIndex f code) of (Just 0) -> f    -- if it was the first, then keep it
                                              Nothing -> f     -- if it wasn't in the string, keep it
                                              _ -> (head code) -- otherwise, it wasn't the first, get the first
