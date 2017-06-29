import Utils.Prime (isPrime)

main = print problem37Value

problem37Value :: Int
problem37Value = sum $ filter isTruncatablePrime candidates

candidates :: [Int]
candidates = map read (twoDigits ++ threeDigits ++ fourDigits ++ fiveDigits ++ sixDigits)
    where twoDigits = [a:b:[] | a <- "2357", b <- "357"]
          threeDigits = [a:b:c:[] | a <- "2357", b <- "1379", c <- "357"]
          fourDigits = [a:b:c:d:[] | a <- "2357", b <- "1379", c <- "1379", d <- "357"]
          fiveDigits = [a:b:c:d:e:[] | a <- "2357", b <- "1379", c <- "1379", d <- "1379", e <- "357"]
          sixDigits = [a:b:c:d:e:f:[] | a <- "2357", b <- "1379", c <- "1379", d <- "1379", e <- "1379", f <- "357"]

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = isRightToLeft n && isLeftToRight n

isRightToLeft :: Int -> Bool
isRightToLeft 0 = True
isRightToLeft n = (isPrime n) && isRightToLeft (n `div` 10)

isLeftToRight :: Int -> Bool
isLeftToRight n
    | n < 10 = isPrime n
    | otherwise = (isPrime n) && (isLeftToRight $ read $ tail $ show n)
