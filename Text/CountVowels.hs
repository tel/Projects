
module Main where

import Data.Char
import qualified Data.Map as Map

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

freq :: (Ord a, Eq a) => [a] -> Map.Map a Int
freq = foldr (Map.alter $ return . maybe 1 succ) Map.empty

printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap = mapM_ printPair . Map.toList where
  printPair (k, v) = putStrLn $ show k ++ " ---> " ++ show v

main :: IO ()
main = printMap . freq . filter isVowel =<< getLine
