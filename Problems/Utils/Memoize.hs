module Utils.Memoize where

import qualified Data.Map as Map

-- memoize takes a function that receives a single parameter and returns a result, and returns a function of the same structure,
-- but where every parameter, result pair is cached so that an operation is only ever done once
memoize :: (a -> b) -> (a -> b)
memoize memFunc = (\x -> )