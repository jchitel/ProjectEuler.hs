main = print problem24Value

problem24Value :: Int
problem24Value = getLexicographicPermutation 999999 [0..9]

-- given an index and a list of digits, compute the lexical (ordered) permutation of those digits at that index
getLexicographicPermutation :: Int -> [Int] -> Int
getLexicographicPermutation index nums = read $ map (head . show) $ getLexicographicPermutation' index nums -- convert a list of numbers to a single number
    where getLexicographicPermutation' _ [] = []
          getLexicographicPermutation' index nums = digit:(getLexicographicPermutation' index' nums')
             where digit = nums !! numIndex                 -- the computed index is the index of the digit to use from the remaining digits
                   numIndex = index `div` factx             -- the index is computed by dividing the index by (n-1)! where n is the length of the remaining list of digits
                   index' = index `mod` factx               -- the new index is the remainder of the division
                   factx = fact ((length nums) - 1)
                   nums' = filter (\x -> x /= digit) nums   -- the new digits list is the previous list with the current digit removed

fact :: Int -> Int
fact num = product [1..num]
