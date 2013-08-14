
module Main where

import Data.List

-- | Fairly naive prime sieve.
primes :: [Int]
primes =
  [ n | n <- [2..]
      , not $ elem n [ j*k | j <- [2..n-1]
                           , k <- [2..n-1] ] ]

-- | If the user hits enter they get another prime. Kill it with C-c.
main :: IO ()
main = mapM_ go primes where
  go prime = print prime >> getLine
