-- Integer operation utilities
module Utils.IntegerOps (intLog, intLogBase, digitAt, triangle, triangles, properDivisors, factors, collatz, divide) where
import Data.Function (on)


intLog :: Integral a => a -> a
intLog a = round $ log $ fromIntegral a

intLogBase :: Integral a => a -> a -> a
intLogBase a b = round $ logBase (fromIntegral a) (fromIntegral b)

digitAt :: Integral a => a -> a -> a
digitAt i num = (num `div` (10 ^ i)) `mod` 10

triangle :: Integral a => a -> a
triangle num = num * (num + 1) `div` 2

triangles :: Integral a => [a]
triangles = map triangle [1..]

properDivisors :: Integral a => a -> [a]
properDivisors num = takeWhile (<num) $ factors num

factors :: Integral a => a -> [a]
factors num
    | isSqrt = lowFactors ++ [sq] ++ (map (\x -> num `div` x) $ reverse lowFactors)
    | otherwise = lowFactors ++ (map (\x -> num `div` x) $ reverse lowFactors)
    where lowFactors = filter (\x -> num `mod` x == 0) [1..cap]
          cap = if isSqrt then sq - 1 else sq
          sq = (floor $ sqrt $ fromIntegral num)
          isSqrt = (sq * sq) == num

collatz :: Integral a => a -> [a]
collatz 1 = []
collatz num = num:(collatz num')
    where num' = if (even num) then (num `div` 2) else (3 * num + 1)

-- because nothing should be this simple
divide :: (Integral a, Fractional b) => a -> a -> b
divide = (/) `on` fromIntegral
