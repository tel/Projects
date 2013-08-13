module Pi where

import Prelude hiding (pi)
import Control.Applicative

arccot :: Int -> Integer -> Integer
arccot n x =
  sum . getZipList
  $ (\x n s -> s $ x `div` n)
     <$> (ZipList $ takeWhile (> 0)
          $ iterate (`div` (x * x)) ((10 ^ n) `div` x))
     <*> ZipList [1,3..]
     <*> ZipList (cycle [id, negate])

pi :: Int -> Integer
pi digits = 4 * (4 * arccot digits 5 - arccot digits 239)
