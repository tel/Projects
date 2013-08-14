
module Factorial where

-- | Fast, memoizing factorial. Not even worth implementing this in
-- loops.
factorial :: Int -> Integer
factorial n = scanl (*) 1 [1..] !! n
