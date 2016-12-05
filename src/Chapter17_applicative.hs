module Chapter17_applicative where

import Control.Applicative

import Data.List (elemIndex)
import Test.QuickCheck hiding (Failure, Success)
import Data.Monoid
import Control.Monad
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function

foo1:: IO String
foo1 = (++) <$> getLine <*> getLine

foo2:: IO String
foo2 = liftA (++) getLine <*> getLine

foo3:: IO Int
foo3 = fmap length $ (++) <$> getLine <*> getLine

foo4:: IO Int
foo4 = liftA2 (\x y -> length (x ++ y)) getLine getLine

-- pure <$> <*>

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z  -- liftA2
  where
    y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- 3
maxed :: Maybe Int
maxed = max <$> xi <*> yi
  where
    xi = elemIndex 3 [1, 2, 3, 4, 5]
    yi = elemIndex 4 [1, 2, 3, 4, 5]

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)
  where
    x = lookup 3 $ zip xs ys
    y = lookup 2 $ zip xs ys

-- identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

-- constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (Constant f) <*> (Constant a) = Constant (mappend f a)

-- 1
lol1 = const <$> Just "Hello" <*> pure "World"
lol2 = liftA2 const (Just "Hello") (pure "World")

-- 2
lol3 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Applicative laws

-- 1. Identity
appIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
appIdentity f = (pure id <*> f) == f

-- 2. Composition
appComposition :: (Applicative f, Eq (f c)) => f (b->c) -> f (a->b) -> f a -> Bool
appComposition u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

-- 3. Homomorphism
appHomomorphism :: (Applicative f, Eq (f b)) => f () -> (a -> b) -> a -> Bool
appHomomorphism proxy f a = (pure f <*> pure a) == (pure (\() x -> x) <*> proxy <*> pure (f a))
-- appHomomorphism [()] (+1) 1
-- appHomomorphism (Just ()) (+1) 1

-- (pure (+1) <*> pure 1) == (pure ((+1) 1) :: Maybe Int)

-- 4. Interchange
appInterchange :: (Applicative f, Eq (f b)) => f (a->b) -> a -> Bool
appInterchange u y = (u <*> pure y) == (pure ($ y) <*> u)

-- list
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ f <$> as

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  fs <*> xs = flatMap (`fmap` xs) fs

toList:: [a] -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a xs) = a : fromList xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = resize 5 $ fmap toList (listOf arbitrary)

instance Eq a => EqProp (List a) where (=-=) = eq

-- zip list

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (repeat a)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipWith (\f v -> f v) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = resize 5 $ fmap ZipList' (listOf arbitrary)

instance Eq a =>
         EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take 3000 l
      ys' =
        let (ZipList' l) = ys
        in take 3000 l

-- validation

data Validation err a
  = Failure err
  | Success a
  deriving (Eq, Show)

data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)
  fmap f (Failure a) = Failure a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success s) = Success (f s)
  (Failure f) <*> (Failure s) = Failure (f <> s)
  (Success f) <*> (Failure s) = Failure s
  (Failure f) <*> (Success s) = Failure f

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [fmap Success arbitrary, fmap Failure arbitrary]
  --oneof [liftM Success arbitrary, liftM Failure arbitrary]

  -- do
  --   a <- arbitrary
  --   b <- arbitrary
  --   elements [Scss a, Error b]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq


-- 1
data Pair a = Pair a a deriving Show
--2
data Two a b = Two a b
--3
data Three a b c = Three a b c
--4
data Three' a b = Three' a b b
--5
data Four a b c d = Four a b c d
--6
data Four' a b = Four' a a a b

--
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a,b,c))

main = do
       quickBatch $ applicative (undefined :: Validation String (String, String, String))
       quickBatch $ applicative (undefined :: List (String, Int, Char))
       quickBatch $ applicative (undefined :: ZipList' (String, Int, Char))
