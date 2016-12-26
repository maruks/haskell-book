module Chapter18_monad where

import Control.Monad

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second b) = Second (f b)
  (Second f) <*> (First a) = First a
  (First f) <*> (Second b) = First f
  (First f) <*> (First a) = First a

instance Monad (Sum a) where
  return = pure
  (Second b) >>= g = g b
  (First a) >>= g = First a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Sum e a) where
  arbitrary = oneof [fmap First arbitrary, fmap Second arbitrary]

instance (Eq e, Eq a) => EqProp (Sum e a) where (=-=) = eq

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

-- 2
data PhEither b a = PhLeft a | PhRight b deriving (Eq, Show)

instance Functor (PhEither b) where
  fmap f (PhLeft a) = PhLeft (f a)
  fmap f (PhRight b) = PhRight b

instance Applicative (PhEither b) where
  pure = PhLeft
  (PhLeft f) <*> (PhLeft a) = PhLeft (f a)
  (PhRight f) <*> _ = PhRight f
  (PhLeft f) <*> (PhRight b) = PhRight b

instance Monad (PhEither b) where
  return = pure
  (PhLeft a) >>= f = f a
  (PhRight b) >>= f = PhRight b

instance (Arbitrary e, Arbitrary a) => Arbitrary (PhEither e a) where
  arbitrary = oneof [fmap PhLeft arbitrary, fmap PhRight arbitrary]

instance (Eq e, Eq a) => EqProp (PhEither e a) where (=-=) = eq

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq
-- 4

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

instance Monad List where
  return = pure
  (>>=) = flip flatMap

toList:: [a] -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a xs) = a : fromList xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = resize 5 $ fmap toList (listOf arbitrary)

instance Eq a => EqProp (List a) where (=-=) = eq

-- Kleisli composition  (<=<) (>=>)

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = g a >>= f

--
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-- 1 join
j01n :: Monad m => m (m a) -> m a
j01n x = x >>= id

--2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = let mb2c = fmap f ma in
               mb2c <*> mb

--4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5
mconz :: Monad m => b -> m [b] -> m [b]
mconz b = fmap (b :)
--  mb >>= (return . (b :))
--         (\bs -> return (b : bs))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = f x >>= (\b -> (b :) <$> meh xs f)
--             f x >>= (\b -> b `mconz` meh xs f)

-- meh [1,2,3] Just  = Just [1,2,3]

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma (join . return)
--                   (\m -> m >>= return m)

--
main = do
       let sum = undefined :: Sum String (String, String, String)
           nope = undefined :: Nope (String, String, String)
           pheither = undefined :: PhEither String (String, String, String)
           ident = undefined :: Identity (String, String, String)
           list = undefined :: List (String, String, String)
       quickBatch $ applicative sum
       quickBatch $ monad sum
       quickBatch $ applicative nope
       quickBatch $ monad nope
       quickBatch $ applicative pheither
       quickBatch $ monad pheither
       quickBatch $ applicative ident
       quickBatch $ monad ident
       quickBatch $ applicative list
       quickBatch $ monad list
