import qualified Data.Set as Set
import Data.List

main = print getProblem29Value

getProblem29Value :: Int
getProblem29Value = numUniques [a^b | a <- [2..100], b <- [2..100]]

numUniques :: [Integer] -> Int
numUniques list = Set.size $ foldl' (\set num -> Set.insert num set) Set.empty list
