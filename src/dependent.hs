{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dependent where

import Prelude hiding (tail, head)

data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m


data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)

infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append (x :- xs) ys = x :- append xs ys
append Nil       ys = ys

    -- Implement the toList and fromList.
    -- Implement the Vector version of map, uncons, init and last.

    -- Implement the zipWithSame, with the following type signature:

    -- zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n

    -- This is the version of the zipWith for vectors with the same length.
    -- Implement the min function for type-level natural numbers. Use it to implement zipWith which takes vectors with possibly different length.

main :: IO ()
main = do
  print $ head (1 :- 2 :- Nil)
  print $ tail (1 :- 2 :- Nil)
  -- | Uncommenting the line below causes type error
  -- print $ head Nil
-- /show
