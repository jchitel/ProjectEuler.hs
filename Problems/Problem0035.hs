import Utils.Prime (primeGen)
import Data.List (foldl', sort)
import qualified Data.Set as Set

main = print problem35Value

problem35Value :: Int
problem35Value = let (_,_,c) = foldl' circularPrimeFold (Set.empty, Set.empty, 0) $ takeWhile (<1000000) primeGen in c

-- Fold function to track and compute circular primes. Tuple is (notVerifiedPrime set, verifiedPrime set, counter) where
-- 'notVerifiedPrime' is the set of rotations of a verified prime number that have not yet been observed in the primes list,
-- 'verifiedPrime' is the set of prime rotations that have not yet been determined circular or not, and
-- 'counter' is the number of observed circular primes.
-- If the prime has no rotations, it is automatically a circular prime.
-- If the prime is in notVerifiedPrime and its rotations are all in verifiedPrime, then all of them are circular primes.
-- If the prime is in notVerifiedPrime otherwise, then it is now to be moved to verifiedPrime.
-- Otherwise, put the prime in verifiedPrime and put all rotations in notVerifiedPrime, because they have not been observed yet.
circularPrimeFold :: (Set.Set Integer, Set.Set Integer, Int) -> Integer -> (Set.Set Integer, Set.Set Integer, Int)
circularPrimeFold (notVerifiedPrime, verifiedPrime, counter) prime
    | length rot == 0                                                                 = (notVerifiedPrime,                  verifiedPrime,                  counter + 1)
    | Set.member prime notVerifiedPrime && all (\x -> Set.member x verifiedPrime) rot = (notVerifiedPrime,                  deleteAll verifiedPrime rot,    counter + (length rot) + 1)
    | Set.member prime notVerifiedPrime                                               = (Set.delete prime notVerifiedPrime, Set.insert prime verifiedPrime, counter)
    | otherwise                                                                       = (insertAll notVerifiedPrime rot,    Set.insert prime verifiedPrime, counter)
    where rot = rotations prime

-- compute all rotations of a number, not including the number itself
rotations :: Integer -> [Integer]
rotations num = let str = (show num) in filter (/=num) $ foldl' (\list i -> (read ((drop i str) ++ (take i str))):list) [] [1..(length str) - 1]

insertAll :: (Set.Set Integer) -> [Integer] -> (Set.Set Integer)
insertAll = foldl' (flip Set.insert)

deleteAll :: (Set.Set Integer) -> [Integer] -> (Set.Set Integer)
deleteAll = foldl' (flip Set.delete)
