
module Main where

{-
    A happy number is defined by the following process. Starting with any
    positive integer, replace the number by the sum of the squares of its
    digits, and repeat the process until the number equals 1 (where it
    will stay), or it loops endlessly in a cycle which does not include
    1. Those numbers for which this process ends in 1 are happy numbers,
    while those that do not end in 1 are unhappy numbers. Take an input
    number from user, and find first 8 happy numbers from that input.
-}

import System.Environment
import qualified Data.Set as Set

step :: Int -> Int
step = sum . map (square . read . return) . show where square x = x*x

amIHappy :: Int -> Bool
amIHappy n = go n Set.empty where
  go 1 _ = True
  go n s = if Set.member n s then False else go (step n) (Set.insert n s)

main :: IO ()
main = do
  args <- getArgs
  print $ case args of
    []  -> go 1
    n:_ -> go (read n)
  where
    go n = take 8 (filter amIHappy [n..])
