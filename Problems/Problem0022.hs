import Data.Char (ord)
import Data.List

main = do
    str <- readFile "Problem0022.txt"
    print $ problem22Value str

problem22Value :: String -> Int
problem22Value str = sum $ getNameScores $ sort $ lines str

getNameScores :: [String] -> [Int]
getNameScores names = getNameScores' 1 names
    where getNameScores' _ [] = []
          getNameScores' i (name:names) = (i * (alphaValue name)):(getNameScores' (i+1) names)

baseAlphaValue :: Int
baseAlphaValue = (ord 'A') - 1

alphaValue :: String -> Int
alphaValue name = sum $ map (\x -> (ord x) - baseAlphaValue) name
