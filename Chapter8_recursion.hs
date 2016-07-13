module Chapter8_recursion where

import Data.List (intersperse)

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = error ">10"

digits :: Int -> [Int]
digits i = go i [] where
  go n a
    | n < 10 = n : a
    | otherwise = go (n `div` 10) (n `mod` 10 : a)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord $ digits n
