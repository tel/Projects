
module Main where

newtype C a = C (a, a) deriving (Eq, Ord, Show)

instance Floating a => Num (C a) where
  C (a1, a2) + C (b1, b2) = C (a1 + b1, a2 + b2)
  C (a1, a2) * C (b1, b2) = C (a1*b1 - a2*b2, a1*b2 + a2*b1)
  C (a1, a2) - C (b1, b2) = C (a1 - b1, a2 - b2)
  negate (C (a, b))       = C (negate a, negate b)
  abs (C (a, b))          = C (sqrt (a*a + b*b), 0)
  signum c                = c / abs c
  fromInteger n = C (fromInteger n, 0)

instance Floating a => Fractional (C a) where
  recip (C (a, b)) = C (a/r, negate b/r) where r = a*a + b*b
  fromRational r   = C (fromRational r, 0)
