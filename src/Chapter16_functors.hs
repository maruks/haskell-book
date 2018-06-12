{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Chapter16_functors where

import GHC.Arr
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Function

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- 1
a = fmap (+1) $ read "[1]" :: [Int]


-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c = fmap (*2) (\x -> x - 2)

-- 4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed


-- functor laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2
data Pair a = Pair a a deriving (Eq,Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3
data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4
data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type IntToInt = Fun Int Int
type IntThree = Three Int Int Int -> IntToInt -> IntToInt -> Bool

-- 5
data Three' a b = Three' a b b deriving (Eq,Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

type IntThree' = Three' Int Int -> IntToInt -> IntToInt -> Bool

-- 6
data Four a b c d = Four a b c d deriving (Eq,Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7
data Four' a b = Four' a a a b deriving (Eq,Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

--
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers v) = Yeppers (f v)
  fmap f LolNope = LolNope

--
data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap f (Left' x) = Left' x
  fmap f (Right' b) = Right' (f b)

--
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)   --  fmap (+1 ) (Wrap (Pair 2 3)) = Wrap (Pair 3 4)

--
getInt :: IO Int
getInt = fmap read getLine

--
bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

bumpIt' :: IO Int
bumpIt' = fmap (+1) getInt

-- natural transformations

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 2
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' (f a)
  fmap f (False' a) = False' (f a)

-- 3
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4
newtype Mu f = InF { outF :: f (Mu f) }
-- kind  (* -> *) -> *

-- 5
data D = D (Array Word Word) Int Int deriving (Eq, Show)
-- kind *

-- 1
data Sum b a = First a | Second b
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2
data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More a b = L b a b | R a b a deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L b a b') = L (f b) a (f b')
  fmap f (R a b a') = R a (f b) a'

-- 1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b = K a

instance Functor (K b) where
  fmap f (K a) = K a

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))
-- fmap (+1) $ Flip (K' 1)

-- 4
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor a => Functor (LiftItOut a) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)

-- fmap (+1) $ LiftItOut (Pair 2 3)

-- 6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- fmap (+1) $ DaWrappa (Pair 1 2) (Pair 3 4)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

-- fmap (+1) $ IgnoringSomething (Pair 1 2) (Pair 3 4)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

--fmap (+1) $ Notorious (Pair 1 2) (Pair 3 4) (Pair 5 6)

-- 9
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- fmap (+1) (Cons 1 (Cons 2 Nil))

-- 10
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)

-- instance Functor ((->) a) where
--   fmap :: (z0 -> z1) -> (a -> z0) -> (a -> f z1)
--   fmap = (.)

--

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d  -- covariant

class Profunctor p where
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d  -- contravariant

instance Profunctor (->) where
  dimap f g h = g . h . f

instance Bifunctor Either where
  bimap f g (Left l) = Left (f l)
  bimap f g (Right r) = Right (g r)

--

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

class ContraFunctor f where
  cmap :: (b -> a) -> f a -> f b

instance ContraFunctor Predicate where
  cmap :: (b -> a) -> Predicate a -> Predicate b
  cmap f (Predicate p) = Predicate (p . f)

--

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Int Int Int)
  quickCheck (functorCompose' :: IntThree)
  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Int Int)
  quickCheck (functorCompose' :: IntThree')
