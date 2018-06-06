module Chapter15_monoids where

import Test.QuickCheck hiding (Failure, Success)
import Data.Monoid
import qualified Data.Semigroup as S
import Data.Semigroup (Semigroup)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada o@(Only a) = o
  mappend o@(Only a) Nada = o
  mappend Nada Nada = Nada
  mappend (Only a) (Only b) = Only $ mappend a b

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do a <- arbitrary
              elements [First' (Only a), First' Nada]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

instance Monoid (First' a) where
  mempty = First' Nada
  mappend o@(First' (Only a)) _ = o
  mappend (First' Nada) o@(First' (Only a)) = o
  mappend (First' Nada) n@(First' Nada) = n

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

-- monoid laws

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- semigroups

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a S.<> b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do a <- arbitrary
                 return (Identity a)

-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = do
--     i <- arbitrary
--     return (Sum i)

type IdentityAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a S.<> c) (b S.<> d )

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a S.<> d) (b S.<> e) (c S.<> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 S.<> a2) (b1 S.<> b2) (c1 S.<> c2) (d1 S.<> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  f@(Snd a) <> _ = f
  (Fst _) <> f@(Snd a) = f
  (Fst _) <> f@(Fst a) = f

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do a <- arbitrary
           b <- arbitrary
           elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

-- 9
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (\a -> f1 a S.<> f2 a)

-- 10
newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp (\a -> f1 a S.<> f2 a)

-- 11
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Validation a b) where
  (Failure a1) <> (Failure a2) = Failure $ a1 S.<> a2
  f@(Failure b) <> (Success a) = f
  (Success a) <> f@(Failure b) = f
  (Success a) <> s@(Success b) = s

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do a <- arbitrary
                   b <- arbitrary
                   elements [Failure a, Success b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool


-- 12
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
         Semigroup (AccumulateRight a b) where
  AccumulateRight (Success a1) <> AccumulateRight (Success a2) = AccumulateRight (Success $ a1 S.<> a2)
  AccumulateRight (Failure b) <> r@(AccumulateRight (Success a)) = r
  s@(AccumulateRight (Success a)) <> AccumulateRight (Failure b) = s
  AccumulateRight (Failure a) <> r@(AccumulateRight (Failure b)) = r

accumulateRightGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
accumulateRightGen = do a <- arbitrary
                        b <- arbitrary
                        elements [AccumulateRight (Failure a), AccumulateRight (Success b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = accumulateRightGen

type AccumulateRightAssoc = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool


-- 13
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success a) <> AccumulateBoth (Success b) = AccumulateBoth (Success $ a S.<> b)
  f@(AccumulateBoth (Failure a)) <> AccumulateBoth (Success b) = f
  AccumulateBoth (Success a) <> f@(AccumulateBoth (Failure b)) = f
  AccumulateBoth (Failure a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure $ a S.<> b)

accumulateBothGen
  :: (Arbitrary a, Arbitrary b)
  => Gen (AccumulateBoth a b)

accumulateBothGen = do
  a <- arbitrary
  b <- arbitrary
  elements [AccumulateBoth (Failure a), AccumulateBoth (Success b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accumulateBothGen

type AccumulateBothAssoc = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool
