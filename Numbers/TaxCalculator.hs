
module TaxCalculator where

main :: IO ()
main = do
  putStrLn "Enter amount."
  n <- readLn
  putStrLn "Enter tax %."
  p <- readLn
  putStr "Total is "
  print $ n * (100 + p)/100
