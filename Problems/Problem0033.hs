import Data.List (foldl')
import Data.Char (digitToInt)

main = print problem33Value

problem33Value :: Int
problem33Value = snd $ reduceFraction $ fractionProduct $ filter isQualified twoDigitFractions

twoDigitFractions :: [(Int, Int)]
twoDigitFractions = [(n, d) | d <- [10..99], n <- [10..99], n < d]

isQualified :: (Int, Int) -> Bool
isQualified (n, d)
    | (sn1 == sd1) && (sn1 /= '0') && (sd2 /= '0') && (compareFracs (n,d) (digitToInt sn2, digitToInt sd2)) = True
    | (sn1 == sd2) && (sn1 /= '0') && (sd1 /= '0') && (compareFracs (n,d) (digitToInt sn2, digitToInt sd1)) = True
    | (sn2 == sd1) && (sn2 /= '0') && (sd2 /= '0') && (compareFracs (n,d) (digitToInt sn1, digitToInt sd2)) = True
    | (sn2 == sd2) && (sn2 /= '0') && (sd1 /= '0') && (compareFracs (n,d) (digitToInt sn1, digitToInt sd1)) = True
    | otherwise                                                                                          = False
    where (sn1:sn2:[], sd1:sd2:[]) = (show n, show d)

compareFracs :: (Int, Int) -> (Int, Int) -> Bool
compareFracs (n1,d1) (n2,d2) = n1*d2 == n2*d1

fractionProduct :: [(Int, Int)] -> (Int, Int)
fractionProduct fracs = foldl' (\(n, d) (n', d') -> (n*n', d*d')) (1,1) fracs

reduceFraction :: (Int, Int) -> (Int, Int)
reduceFraction (n, d) = let g = gcd n d in (n `div` g, d `div` g)
