
import Utils.IntegerOps


main = print getProblem12Value

getProblem12Value :: Integer
getProblem12Value = head $ filter (\x -> (length $ factors x) > 500) triangles