main = print problem206Value

problem206Value :: Integer
problem206Value = head $ filter hasMatchingSquare [1010101010,1010101020..1389026620]

hasMatchingSquare :: Integer -> Bool
hasMatchingSquare n = (evens $ show square) == "1234567890"
    where square = n ^ 2
          evens str = map (str!!) $ filter even [0..(length str)-1]
