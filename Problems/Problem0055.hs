main = print problem55Value

problem55Value :: Int
problem55Value = length [n | n <- [1..9999], isLychrel n 0]

-- iterate until 50, adding the number to itself reversed until the sum is palindromic.
-- if the sum is ever palindromic, return false, otherwise it's true
isLychrel :: Integer -> Int -> Bool
isLychrel n i
    | i == 50           = True
    | isPalindromic sum = False
    | otherwise         = isLychrel sum (i+1)
    where sum             = n + (read $ reverse $ show n)
          isPalindromic n = let sn = (show n) in (sn == reverse sn)
