module Chapter17_applicative where

import Control.Applicative

import Data.List (elemIndex)
import Test.QuickCheck hiding (Failure, Success)
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
  fs <*> xs = flatMap (\f -> fmap f xs) fs
