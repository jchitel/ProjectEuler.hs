import Utils.ListOps (filterMap)
import Data.List (permutations, nub)

main = print problem32Value

problem32Value :: Integer
problem32Value = sum $ nub pandigitalProducts

pandigitalProducts :: [Integer]
pandigitalProducts = filterMap getPandigitalProduct $ permutations "123456789"

getPandigitalProduct :: String -> Maybe Integer
getPandigitalProduct str
    | a * b == c = Just c
    | a' * b' == c = Just c
    | otherwise = Nothing
    where d:e:f:g:h:i:j:k:l:[] = str
          a = read [d,e]
          b = read [f,g,h]
          a' = read [d]
          b' = read [e,f,g,h]
          c = read [i,j,k,l]
