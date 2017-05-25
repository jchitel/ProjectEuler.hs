module Utils.Fibonacci (fibSequence) where

fibSequence :: [Integer]
fibSequence = 0 : 1 : zipWith (+) fibSequence (tail fibSequence)
