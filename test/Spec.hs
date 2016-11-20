module Main where

import Data.List (sort,nub)
import Test.QuickCheck
import Chapter11_adt
import Chapter15_monoids

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
  -- monoids
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
  -- semigroups
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
