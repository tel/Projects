
module Main where

-- | http://en.wikipedia.org/wiki/Luhn_algorithm
luhn :: [Int] -> Bool
luhn = (==0) . (`mod` 10) . fst . foldl go (0, False) . reverse where
  go :: (Int, Bool) -> Int -> (Int, Bool)
  go (sum, doubling) x = (if doubling then sum + doubleUp x else sum + x, not doubling)
  doubleUp x =
    let y = 2*x
    in if y > 9 then sum (digits y) else y

digits :: Int -> [Int]
digits = map (read . return) . show
         
valid :: [Int]
valid = digits 79927398713
