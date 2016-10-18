module Main where

import Data.List (sort,nub)
import Test.QuickCheck
import Chapter11_adt

propSorted :: [Int] -> Bool
propSorted xs = inorder_elems == sorted where
  inorder_elems = inorder $ fromList xs
  sorted = sort $ nub xs

propMapped :: [Int] -> Bool
propMapped xs = mapped_elems == mapped where
  f = (+3)
  mapped = sort $ map f $ nub xs
  mapped_elems = inorder $ mapTree f $ fromList $ nub xs

main :: IO ()
main = do
  quickCheck propSorted
  quickCheck propMapped
