module Chapter15_monoids_2 where

import Test.QuickCheck hiding (Failure, Success)
import Data.Monoid

-- monoids

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq,Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity $ a `mappend` b

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type IdentityId = Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq,Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  (Two a b) `mappend` (Two c d) = Two (a `mappend` c) (b `mappend` d )

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type TwoId = Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq,Show)

instance Monoid BoolConj where
  mempty = BoolConj True
  (BoolConj a) `mappend` (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool

-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  (BoolDisj a) `mappend` (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool

-- 6
newtype Combine a b = Combine { unCombine :: a -> b }

instance Show (Combine a b) where
  show _ = "Combine"

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend (Combine f) (Combine g) = Combine (\x -> mappend (f x) (g x))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do a <- arbitrary
                 return (Combine a)

monoidAssocCombine :: Combine String String -> Combine String String -> Combine String String -> Bool
monoidAssocCombine a b c = unCombine (a <> (b <> c)) "a" == unCombine ((a <> b) <> c) "a"

monoidLeftIdCombine :: Combine String String -> Bool
monoidLeftIdCombine a = (unCombine (mempty <> a)) "a" == unCombine a "a"

monoidRightIdCombine :: Combine String String -> Bool
monoidRightIdCombine a = (unCombine (a <> mempty)) "a" == unCombine a "a"


-- 7
newtype Comp a = Comp (a -> a)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend (Comp f) (Comp g) = Comp (f . g)

-- 8
newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a =>
         Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) =
    Mem
      (\s ->
         let (a1, s1) = f s
             (a2, s2) = g s1
         in (mappend a1 a2, s2))
---

main :: IO ()
main = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: IdentityId)
  quickCheck (monoidRightIdentity :: IdentityId)
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoId)
  quickCheck (monoidRightIdentity :: TwoId)
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConjId)
  quickCheck (monoidRightIdentity :: BoolConjId)
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisjId)
  quickCheck (monoidRightIdentity :: BoolDisjId)
  quickCheck monoidAssocCombine
  quickCheck monoidLeftIdCombine
  quickCheck monoidRightIdCombine
