
module WordNumbers where

-- A beautiful approach from Chung-chieh Shan
-- (http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers1/)

import Prelude hiding ((+), (*), sum, product)
import qualified Prelude as P

import Control.Monad

infixl 6 +
infixl 7 *

-- | Define Monoids clearly as the commutative part of a
-- (semi)(near)ring
class Monoid a where
  zero :: a
  (+) :: a -> a -> a

-- | The Free construction
instance Monoid [a] where
  zero = []
  (+) = (++)

sum :: (Monoid a) => [a] -> a
sum = foldr (+) zero

-- | Append another non-commutative monoid to get a seminearring.
-- 
-- A seminearring is a 'Monoid' with an additional associative
-- operation '(*)' and its identity element 'one' satisfying
-- distributivity on one side only.
-- 
--     (x+y) * z = x*z + y*z
class (Monoid a) => Seminearring a where
  one :: a
  (*) :: a -> a -> a

product :: (Seminearring a) => [a] -> a
product = foldr (*) one

instance Seminearring [[a]] where
  one = [[]]
  xss * yss = [ xs ++ ys | xs <- xss, ys <- yss ]

string :: String -> [String]
string = product . map (\x -> [[x]])

strings :: String -> [String]
strings = sum . map string . words

ten1, ten2, ten3, ten6, ten9 :: [String]
ten1 = strings "one two three four five six seven eight nine"
ten2 = ten1 + strings "ten eleven twelve"
            + (strings "thir four"     + prefixes) * string "teen"
            + (strings "twen thir for" + prefixes) * string "ty" * (one + ten1)
    where prefixes = strings "fif six seven eigh nine"
ten3 = ten2 + ten1 * string "hundred" * (one + ten2)
ten6 = ten3 + ten3 * string "thousand" * (one + ten3)
ten9 = ten6 + ten3 * string "million" * (one + ten6)

main :: IO ()
main = forever $ readLn >>= print . (("zero":ten9) !!)
