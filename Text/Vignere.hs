{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

{- Vigenere / Vernam / Ceasar Ciphers -}

{-

  The Ceasar cipher is simply a shift operation on an ASCII ring. The
Vernam cipher is a One Time Pad. The Vigenere cipher is just the
Ceasar cipher where the shift value is unique for each letter, derived
from a key.

-}

import Data.Char

-- | Z_26 -- characters as a cyclic group
newtype Alpha = Alpha { asInt :: Int } deriving (Eq)

alpha :: Int -> Alpha
alpha n = if n > 0 then go n else alpha (n + 26) where
  go = Alpha . (`mod` 26)

normalize :: Alpha -> Alpha
normalize = alpha . asInt

fromChr :: Char -> Alpha
fromChr = alpha . (flip (-) 97) . ord . toLower

toChr :: Alpha -> Char
toChr (Alpha n) = chr (n + 97)

overAlpha :: (Alpha -> Alpha) -> (Char -> Char)
overAlpha f = toChr . normalize . f . fromChr

shift :: Int -> Alpha -> Alpha
shift m (Alpha n) = alpha (n + m)

(#+) :: Alpha -> Alpha -> Alpha
(Alpha n1) #+ (Alpha n2) = alpha (n1 + n2)

(#-) :: Alpha -> Alpha -> Alpha
(Alpha n1) #- (Alpha n2) = alpha (n1 - n2)

instance Show Alpha where
  show = return . toChr

ceasarCipher :: Int -> [Char] -> [Char]
ceasarCipher n   = map (overAlpha (shift n))

ceasarDecipher :: Int -> [Char] -> [Char]
ceasarDecipher n = map (overAlpha (shift (negate n)))

vernamCipher :: [Char] -> [Char] -> [Char]
vernamCipher = zipWith go where
  go k c = toChr $ fromChr c #+ fromChr k

vernamDecipher :: [Char] -> [Char] -> [Char]
vernamDecipher = zipWith go where
  go k c = toChr $ fromChr c #- fromChr k

vigenereCipher :: [Char] -> [Char] -> [Char]
vigenereCipher key = zipWith go (cycle key) where
  go k c = toChr $ shift (asInt $ fromChr k) (fromChr c)

vigenereDecipher :: [Char] -> [Char] -> [Char]
vigenereDecipher key = zipWith go (cycle key) where
  go k c = toChr $ shift (negate $ asInt $ fromChr k) (fromChr c)

