{-# LANGUAGE BangPatterns #-}

module Utils.Memo (memoList, memoTree, toList) where

-- List Memoization

-- Mutually recursive function that memoizes an operation by storing intermediate results in a list
memoList :: (Integral a) => ((a -> a) -> a -> a) -> a -> a
memoList f = memoList_f
  where memoList_f = (memo !!) . fromIntegral
        memo = map (f memoList_f) [0..]

-- Usage: (f) is an open-recursive function that uses the provided (mf) for recursion
--
-- f :: (Integer -> Integer) -> Integer -> Integer
-- f mf n = ...
--
-- (faster_f) creates the actual memoized function by passing (f) to (memoList)
-- 
-- faster_f :: Integer -> Integer
-- faster_f = memoList f


-- Tree Memoization (for when a list just doesn't cut it)

-- Infinite tree data structure (index representation below):
--             0
--      1             2
--   3      4     5       6
-- 7   8  9  10 11 12  13   14
data MemoTree a = MemoTree (MemoTree a) a (MemoTree a)
instance Functor MemoTree where
    fmap f (MemoTree l m r) = MemoTree (fmap f l) (f m) (fmap f r)

-- get a value from the tree at a specific index
index :: (Integral a) => MemoTree a -> a -> a
index (MemoTree _ m _) 0 = m
index (MemoTree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

-- same as [0..] but for the MemoTree data structure
nats :: (Integral a) => MemoTree a
nats = go 0 1
    where
        go !n !s = MemoTree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

-- convert the tree to a list
toList :: (Integral a) => MemoTree a -> [a]
toList as = map (index as) [0..]

-- Mutually recursive function that memoizes an operation by storing intermediate results in a tree
memoTree :: (Integral a) => ((a -> a) -> a -> a) -> a -> a
memoTree f = memoTree_f
  where memoTree_f = index memo
        memo = fmap (f memoTree_f) nats

-- Usage: (f) is an open-recursive function that uses the provided (mf) for recursion
--
-- f :: (Integer -> Integer) -> Integer -> Integer
-- f mf n = ...
--
-- (fastest_f) creates the actual memoized function by passing (f) to (memoTree)
-- 
-- fastest_f :: Integer -> Integer
-- fastest_f = memoTree f