-- Integer operation utilities

module Utils.IntegerOps (intLog, intLogBase) where

intLog :: Integral a => a -> a
intLog a = round $ log $ fromIntegral a

intLogBase :: Integral a => a -> a -> a
intLogBase a b = round $ logBase (fromIntegral a) (fromIntegral b)

digitAt :: Integral a => a -> a -> a
digitAt i num = (num `div` (10 ^ i)) `mod` 10
