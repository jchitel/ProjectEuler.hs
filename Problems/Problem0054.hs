import Data.List (sortBy, foldl, group)

main = do
    str <- readFile "Problem0054.txt"
    print $ problem54Value str

-- split the input string into lines, determine if player 1 wins for each line, and return the number of times player 1 won
problem54Value :: String -> Int
problem54Value str = length $ filter player1Wins $ lines str

-- This type has the correct ordering of ranks (and component card values) so that we don't need to explicitly implement Ord and Eq.
-- This is the meaning of each component of each value constructor:
--              all cards        pair, remainder     pair1, pair2, remain   3 card, remaining 2      high card      all cards     3 card, 2 card      4 card, remaining 1   high card           n/a
data HandRank = HighCard [Int] | OnePair Int [Int] | TwoPairs Int Int Int | ThreeOfAKind Int [Int] | Straight Int | Flush [Int] | FullHouse Int Int | FourOfAKind Int Int | StraightFlush Int | RoyalFlush
    deriving (Ord, Eq, Show)

-- player 1 wins if his rank is greater than player 2's rank
player1Wins :: String -> Bool
player1Wins hand = p1Hand > p2Hand
    where cards = words hand
          p1Hand = assignRank $ map parseCard $ take 5 cards
          p2Hand = assignRank $ map parseCard $ drop 5 cards

-- convert numeric card values to numbers, convert other card values to corresponding numbers, keep the suit as a Char
parseCard :: String -> (Int, Char)
parseCard (vs:s:[]) = (v, s)
    where v = if (vs >= '2' && vs <= '9') then (read [vs]) else case vs of { 'A' -> 14; 'T' -> 10; 'J' -> 11; 'Q' -> 12; 'K' -> 13 }

-- This is where the magic happens.
-- Each possible rank of a hand is checked from best to worst.
-- The first rank that matches is the rank of the hand.
assignRank :: [(Int, Char)] -> HandRank
assignRank hand
    | values == [14,13..10] && isFlush                           = RoyalFlush                                          -- Royal flush: the hand is a flush and it is the highest possible straight
    | values == [highCard,(highCard-1)..(highCard-4)] && isFlush = StraightFlush highCard                              -- Straight flush: hand is a flush and a straight
    | groupSizes == [4,1]                                        = FourOfAKind (groups !! 0) (groups !! 1)             -- 4 of a kind: there is a group of 4 of the same card
    | groupSizes == [3,2]                                        = FullHouse (groups !! 0) (groups !! 1)               -- Full house: there is a group of 3 and a pair
    | isFlush                                                    = Flush values                                        -- Flush: all cards of the same suit
    | values == [highCard,(highCard-1)..(highCard-4)]            = Straight highCard                                   -- Straight: cards are in incrementing order
    | groupSizes == [3,1,1]                                      = ThreeOfAKind (groups !! 0) (tail groups)            -- 3 of a kind: there is a group of 3
    | groupSizes == [2,2,1]                                      = TwoPairs (groups !! 0) (groups !! 1) (groups !! 2)  -- 2 pair: there are two different groups of 2
    | groupSizes == [2,1,1,1]                                    = OnePair (groups !! 0) (tail groups)                 -- Pair: there is a group of 2
    | otherwise                                                  = HighCard values                                     -- High card: if nothing else, the highest card determines the rank
    where sorted = reverse $ sortBy (\(v, s) (v1, s1) -> compare v v1) hand -- compare the cards by value (descending order)
          values = map fst sorted                                           -- list of just the values
          suits = map snd sorted                                            -- list of just the suits
          highCard = head values                                            -- the highest card value
          (groups, groupSizes) = getValueGroups values                      -- the grouped values (for things like 4-of-a-kind, full-house, etc.)
          isFlush = and $ map (== head suits) (tail suits)                  -- the hand is a flush if all cards are of the same suit

-- takes a sorted list of card values, returns a tuple with:
-- 1. List of component values of each group of values, sorted descending by the size of the group
-- 2. List of sizes of each group
-- Ex: This list of values [5,5,3,3,3] will return ([3,5], [3,2])
-- because there are two groups, and the one containing 3's is larger, so it is put first
getValueGroups :: [Int] -> ([Int], [Int])
getValueGroups values = (groupMembers, sortedSizes)
    where groups = group values
          sortedGroups = sortBy (\g g1 -> compare (length g1) (length g)) groups -- swap the groups in the sort so we get descending order
          groupMembers = map head sortedGroups
          sortedSizes = map length sortedGroups
