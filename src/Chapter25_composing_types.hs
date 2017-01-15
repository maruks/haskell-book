{-# LANGUAGE InstanceSigs #-}
module Chapter25_composing_types where

import Control.Monad.Identity
import Control.Monad.Trans.Identity

import Control.Applicative (liftA2)
import Control.Monad

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- fmap (+1) (Compose [Just 1, Nothing])

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a  -- (<*>) <$> f <*> a

-- pure (+1) <*> (Compose [Just 1, Nothing])

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m, Foldable f, Foldable g) => (a -> m) -> Compose f g a -> m
  foldMap = undefined

instance (Traversable fc, Traversable gc) => Traversable (Compose fc gc) where
  traverse :: (Traversable fc, Traversable gc, Applicative f) => (a -> f b) -> Compose fc gc a -> f (Compose fc gc b)
  traverse = undefined

--
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

--1.
data Deux a b = Deux a b deriving(Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

--2.
data Const a b = Const a deriving(Eq, Show)

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

--3.
data Drei a b c = Drei a b c deriving(Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

--4.
data SuperDrei a b c = SuperDrei a b deriving(Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

--5.
data SemiDrei a b c = SemiDrei a deriving(Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

--6.
data Quadriceps a b c d = Quadzzz a b c d deriving(Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

--7.
data Either2 a b = Left2 a | Right2 b deriving(Eq, Show)

instance Bifunctor Either2 where
  bimap f g e = case e of
                  (Left2 a) -> Left2 $ f a
                  (Right2 b) -> Right2 $ g b
