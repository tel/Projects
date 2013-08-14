
module Main where

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main :: IO ()
main = do
  getLine >>= print . isPalindrome
