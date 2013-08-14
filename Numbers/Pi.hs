{-# LANGUAGE DeriveFunctor #-}
module Main where

import Prelude hiding (pi)
import System.Environment
import Control.Applicative
import Control.Parallel

-- | A ziplist which evaluates its list in parallel
newtype PZipList a = PZL { unPZL :: [a] }
                   deriving (Eq, Ord, Show, Functor)

instance Applicative PZipList where
  pure      = PZL . pure
  (PZL fs) <*> (PZL xs) = PZL $ par xs (zipWith ($) fs xs)

-- | Lazily generate terms of arc-cotangent. The 'ZipList's expose an
-- 'Applicative' interface, so we run them in parallel and then zip up
-- the terms in-between.
arccot :: Int -> Integer -> Integer
arccot n x =
  sum . unPZL
  $ (\x n s -> s $ x `div` n)
     <$> (PZL $ takeWhile (> 0)
          $ iterate (`div` (x * x)) ((10 ^ n) `div` x))
     <*> PZL [1,3..]
     <*> PZL (cycle [id, negate])

pi :: Int -> Integer
pi digits = 4 * (4 * arccot digits 5 - arccot digits 239)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> print $ pi 100
    n:_ -> print $ pi (read n)
