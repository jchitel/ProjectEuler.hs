{-
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-}
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
