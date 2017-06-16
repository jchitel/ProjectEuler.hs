import Utils.Fibonacci
import Data.List

main = print problem25Value

problem25Value :: Int
problem25Value = let (Just index) = findIndex (\x -> (length $ show x) == 1000) fibSequence in index
