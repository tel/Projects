module Main where

import System.Environment (getArgs)

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main = do
  args <- getArgs
  case args of
    []  -> print $ fibs !! 100
    x:_ -> print $ fibs !! read x
