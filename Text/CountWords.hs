
module Main where

import Data.List
import Data.Ord
import qualified Data.Map as Map

freq :: (Ord a, Eq a) => [a] -> Map.Map a Int
freq = foldr (Map.alter $ return . maybe 1 succ) Map.empty

printPair :: (Show k, Show v) => (k, v) -> IO ()
printPair (k, v) = putStrLn $ show k ++ " ---> " ++ show v


main :: IO ()
main =
  getContents >>= mapM_ printPair . take 10
                  . sortBy (comparing snd)
                  . Map.toList . freq . words
